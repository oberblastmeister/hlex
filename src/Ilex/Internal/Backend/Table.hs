{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Backend.Table
  ( matchesDfa,
    codegen,
  )
where

import Control.Monad.State.Strict qualified as State
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Primitive (ByteArray#)
import Data.Primitive qualified as Primitive
import Data.Vector qualified as V
import Data.Vector qualified as VB
import Data.Vector.Storable qualified as VS
import Foreign qualified
import Foreign.Storable qualified as Storable
import GHC.Exts (Addr#, Int (..), Int#, (*#), (+#), (>=#))
import GHC.Exts qualified as Exts
import GHC.Int (Int32 (I32#))
import GHC.Word (Word32 (W32#), Word8 (W8#))
import Ilex.Internal.Backend
import Ilex.Internal.Dfa (Dfa)
import Ilex.Internal.Dfa qualified as Dfa
import Ilex.Internal.Monad
import Ilex.Internal.Prim (unI#)
import Ilex.Internal.Rule qualified as Rule
import Language.Haskell.TH qualified as TH

-- (state, byte) -> state
generateTransitionTable :: Dfa a -> VS.Vector Int32
generateTransitionTable dfa =
  VS.fromList
    [ fromIntegral @Int @Int32 to
      | state <- V.toList $ Dfa.states dfa,
        (_, to) <- expandErrorStates $ Dfa.transitions state
    ]

-- state -> info
generateInfoTable :: Dfa a -> VS.Vector Word32
generateInfoTable dfa =
  VS.fromList
    [ (fromIntegral acceptId `Bits.shiftL` 1)
        Bits..|. (if Dfa.isCharEnd state then 1 else 0)
      | (acceptId, state) <- zipAcceptIds $ V.toList $ Dfa.states dfa
    ]

generateIsAcceptTable :: Dfa a -> VS.Vector Word8
generateIsAcceptTable dfa =
  VS.fromList
    [ if Maybe.isJust $ Dfa.accept state then 1 else 0
      | state <- V.toList $ Dfa.states dfa
    ]

zipAcceptIds :: [Dfa.State a] -> [(Int, Dfa.State a)]
zipAcceptIds = go 0
  where
    go _ [] = []
    go i (state : states) =
      case Dfa.accept state of
        Nothing -> (-1, state) : go i states
        Just _ -> (i, state) : go (i + 1) states

storableVectorToByteString :: forall a. Storable.Storable a => VS.Vector a -> ByteString
storableVectorToByteString v =
  let (fp, len) = VS.unsafeToForeignPtr0 v
   in B.Internal.fromForeignPtr0 (Foreign.castForeignPtr fp) (len * Storable.sizeOf (undefined :: a))

liftStorableVectorToAddr# :: Storable.Storable a => VS.Vector a -> TH.ExpQ
liftStorableVectorToAddr# = TH.litE . TH.stringPrimL . B.unpack . storableVectorToByteString

shrinkAccepts :: NonEmpty Rule.Accept -> NonEmpty Rule.Accept
shrinkAccepts (a NE.:| as)
  | Rule.Accept {context = Nothing} <- a = a NE.:| []
  | otherwise = a NE.:| foldr f as []
  where
    f a@Rule.Accept {context = Nothing} _as = [a]
    f a as = a : as

genCheckPredicatesExpCase :: TH.Name -> TH.Name -> Int -> NonEmpty (Int, Rule.Accept) -> TH.MatchQ
genCheckPredicatesExpCase inputName inputRestName acceptId toRun =
  TH.match
    (TH.litP (TH.intPrimL $ toInteger acceptId))
    (TH.normalB $ go contextToId)
    []
  where
    contextToId =
      [ (context, runId)
        | (runId, Rule.Accept {context}) <- NE.toList toRun
      ]
    go :: [(Maybe TH.ExpQ, Int)] -> TH.ExpQ
    go [] = [|NoRunId#|]
    go ((context, runId) : contexts) = case context of
      Nothing -> [|SomeRunId# $(TH.litE $ TH.intPrimL $ toInteger runId)|]
      Just context ->
        [|
          if $(context) $(TH.varE inputName) $(TH.varE inputRestName)
            then SomeRunId# $(TH.litE $ TH.intPrimL $ toInteger runId)
            else $(go contexts)
          |]

emptyCheckPredicatesExp :: TH.ExpQ
emptyCheckPredicatesExp =
  [|
    \(acceptId :: Int#) (_ :: Pos#) (_ :: Pos#) (_ :: Input# u) -> SomeRunId# acceptId
    |]

genCheckPredicatesExp :: Dfa (NonEmpty Rule.Accept) -> TH.ExpQ
genCheckPredicatesExp dfa = do
  inputName <- TH.newName "input"
  inputRestName <- TH.newName "inputRest"
  acceptIdName <- TH.newName "acceptId"
  let matches = [genCheckPredicatesExpCase inputName inputRestName acceptId toRun | (acceptId, toRun) <- withAcceptIds]
      caseExp = TH.caseE (TH.varE acceptIdName) matches
  [|
    \($(TH.varP acceptIdName) :: Int#) (start :: Pos#) (end :: Pos#) (input :: Input# u) ->
      let $(TH.varP inputName) =
            liftInput# @u
              input
                { inputStart# = off# start,
                  inputEnd# = off# end
                }
          $(TH.varP inputRestName) =
            liftInput# @u input {inputStart# = off# end}
       in $caseExp
    |]
  where
    accepts = [a | Dfa.State {accept = Just a} <- V.toList $ Dfa.states dfa]
    withRunIds =
      flip State.evalState (0 :: Int) $
        (traverse . traverse)
          (\a -> do i <- State.get; State.put $! i + 1; pure (i, a))
          accepts
    withAcceptIds = zip [0 :: Int ..] withRunIds

type Matches# = Addr# -> Addr# -> Int# -> ByteArray# -> Int# -> Int# -> Int#

matchesDfa :: Dfa a -> TH.ExpQ
matchesDfa dfa =
  [|
    let stateTable = $(liftStorableVectorToAddr# $ generateTransitionTable dfa)
        isAcceptTable = $(liftStorableVectorToAddr# $ generateIsAcceptTable dfa)
     in \Input {inputArr = Primitive.ByteArray inputArr#, inputStart = I# off#, inputEnd = I# inputEnd#} ->
          Exts.isTrue# (matches# stateTable isAcceptTable $(TH.litE $ TH.intPrimL $ toInteger $ Dfa.start dfa) inputArr# off# inputEnd#)
    |]

matches# :: Matches#
matches# stateTable isAcceptTable = go
  where
    go stateId arr off endOff =
      case W8# (Exts.indexWord8OffAddr# isAcceptTable stateId) of
        1 -> 1#
        _ ->
          case off >=# endOff of
            1# -> 0#
            _ -> do
              let !b = W8# (Exts.indexWord8Array# arr off)
                  !stateId' = nextState# stateTable stateId b
              case stateId' of
                -1# -> 0#
                _ -> go stateId' arr (off +# 1#) endOff
{-# INLINE matches# #-}

-- FIXME: error stuff can go through char boundaries
codegen :: BackendConfig -> Dfa (NonEmpty Rule.Accept) -> TH.ExpQ
codegen config dfa = do
  dfa <- pure $ shrinkAccepts <$> dfa
  let acceptMap = VB.fromList $ fmap Rule.exp $ concatMap NE.toList $ Dfa.accepts dfa
      onInvalidUtf8Exp = case onInvalidUtf8 config of
        -- this error will only happen when lexing bytes, so that it should be okay
        -- people that lex bytestring should specifically provide the onInvalidUtf8
        Nothing -> [|error "invalid utf8"|]
        Just exp -> exp
      allSingle = all (\case _ NE.:| as -> null as) dfa
      checkPredicatesExp =
        if allSingle
          then emptyCheckPredicatesExp
          else genCheckPredicatesExp dfa
  decs <-
    [d|
      acceptSwitch = $(acceptSwitchExp acceptMap)

      stateTable = $(liftStorableVectorToAddr# $ generateTransitionTable dfa)

      infoTable = $(liftStorableVectorToAddr# $ generateInfoTable dfa)

      runOnInvalidUtf8 = $(withInpExp onInvalidUtf8Exp)

      runOnEof = $(withInpExp (onEof config))

      checkPredicates = $checkPredicatesExp

      loop (stateId :: Int#) (pos :: Pos#) (lastMatch :: LastMatch#) =
        withPos $ \start ->
          withInput $ \(input@Input# {inputArr#, inputEnd#} :: Input# u) ->
            let !info = W32# (Exts.indexWord32OffAddr# infoTable stateId)
                !acceptId = unI# (fromIntegral @Int32 @Int ((fromIntegral @Word32 @Int32 info) `Bits.unsafeShiftR` 1))
                !newPos = pos {charOff# = charOff# pos +# unI# (fromIntegral @Word32 @Int (info Bits..&. 1))}
                !newMatch = case acceptId of
                  -1# -> lastMatch
                  _ ->
                    case checkPredicates acceptId start newPos input of
                      NoRunId# -> lastMatch
                      SomeRunId# runId ->
                        SomeLastMatch#
                          { lastMatchAccept# = runId,
                            lastMatchEnd# = newPos
                          }
             in case off# pos >=# inputEnd# of
                  1# -> case newMatch of
                    NoLastMatch# -> runOnEof newPos
                    SomeLastMatch# {lastMatchAccept#, lastMatchEnd#} ->
                      acceptSwitch lastMatchAccept# lastMatchEnd#
                  _ ->
                    let !b = W8# (Exts.indexWord8Array# inputArr# (off# pos))
                        !stateId' = nextState# stateTable stateId b
                     in case stateId' of
                          -1# ->
                            case newMatch of
                              NoLastMatch# -> runOnInvalidUtf8 newPos {off# = off# newPos +# 1#}
                              SomeLastMatch# {lastMatchAccept#, lastMatchEnd#} ->
                                acceptSwitch lastMatchAccept# lastMatchEnd#
                          _ -> loop stateId' newPos {off# = off# newPos +# 1#} newMatch
      |]
  body <-
    [|
      withPos $ \pos ->
        loop $(TH.litE $ TH.intPrimL $ toInteger $ Dfa.start dfa) pos NoLastMatch#
      |]
  pure $ TH.LetE decs body

nextState# :: Addr# -> Int# -> Word8 -> Int#
nextState# table stateId b =
  unI#
    ( fromIntegral @Int32 @Int $
        I32#
          ( Exts.indexInt32OffAddr#
              table
              (256# *# stateId +# unI# (fromIntegral @Word8 @Int b))
          )
    )
{-# INLINE nextState# #-}
