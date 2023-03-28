{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Backend.Table where

import Control.Monad.State.Strict qualified as State
import Data.Bits qualified as Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Traversable (for)
import Data.Vector qualified as V
import Data.Vector qualified as VB
import Data.Vector.Storable qualified as VS
import Foreign qualified
import Foreign.Storable qualified as Storable
import GHC.Exts (Int (I#), Int#, (*#), (+#), (>=#))
import GHC.Exts qualified as Exts
import GHC.Int (Int32 (I32#))
import GHC.Word (Word32 (W32#), Word8 (W8#))
import Ilex.Internal.Backend
import Ilex.Internal.Dfa (Dfa)
import Ilex.Internal.Dfa qualified as Dfa
import Ilex.Internal.Monad
import Ilex.Internal.Rule (Rule)
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
generateInfoTable :: Dfa Int -> VS.Vector Word32
generateInfoTable dfa =
  VS.fromList
    [ ( ( case Dfa.accept state of
            Nothing -> fromIntegral @Int @Word32 (-1)
            Just acceptId -> fromIntegral acceptId
        )
          `Bits.shiftL` 1
      )
        Bits..|. (if Dfa.isCharEnd state then 1 else 0)
      | state <- V.toList $ Dfa.states dfa
    ]

storableVectorToByteString :: forall a. Storable.Storable a => VS.Vector a -> ByteString
storableVectorToByteString v =
  let (fp, len) = VS.unsafeToForeignPtr0 v
   in B.Internal.fromForeignPtr0 (Foreign.castForeignPtr fp) (len * Storable.sizeOf (undefined :: a))

liftStorableVectorToAddr# :: Storable.Storable a => VS.Vector a -> TH.ExpQ
liftStorableVectorToAddr# = TH.litE . TH.stringPrimL . B.unpack . storableVectorToByteString

shrinkAccepts :: NonEmpty Rule.Accept -> NonEmpty Rule.Accept
shrinkAccepts (a NE.:| as)
  | Rule.Accept {context = []} <- a = a NE.:| []
  | otherwise = a NE.:| foldr f as []
  where
    f a@Rule.Accept {context = []} _as = [a]
    f a as = a : as

genCheckPredicatesExp :: [Rule.Accept] -> TH.ExpQ
genCheckPredicatesExp = undefined

-- FIXME: error stuff can go through char boundaries
codegen :: BackendConfig -> Dfa (NonEmpty Rule.Accept) -> TH.ExpQ
codegen config dfa = do
  dfa <- pure $ shrinkAccepts <$> dfa
  let dfaWithIds = flip State.evalState (0 :: Int) $ for dfa \_ -> do
        i <- State.get
        State.put $! i + 1
        pure i
      acceptMap = VB.fromList $ fmap Rule.exp $ concatMap NE.toList $ Dfa.accepts dfa
      allSingle = all (\case _ NE.:| as -> null as) dfa
      checkPredicatesExp =
        if allSingle
          then undefined
          else undefined
  decs <-
    [d|
      acceptSwitch = $(acceptSwitchExp acceptMap)

      stateTable = $(liftStorableVectorToAddr# $ generateTransitionTable dfa)

      infoTable = $(liftStorableVectorToAddr# $ generateInfoTable dfaWithIds)

      runOnError = $(withInpExp (onError config))

      runOnEof = $(withInpExp (onEof config))

      checkPredicates = undefined

      loop (stateId :: Int#) (lexerState :: LexerState#) (lastMatch :: LastMatch#) =
        withLexerEnv $ \LexerEnv {arr#, endOff#} ->
          let !info = W32# (Exts.indexWord32OffAddr# infoTable stateId)
              !acceptId = unI# (fromIntegral @Int32 @Int ((fromIntegral @Word32 @Int32 info) `Bits.unsafeShiftR` 1))
              !newLexerState = lexerState {charOff# = charOff# lexerState +# unI# (fromIntegral @Word32 @Int (info Bits..&. 1))}
              !newMatch = case acceptId of
                -1# -> lastMatch
                _ ->
                  SomeLastMatch#
                    { lastMatchAccept# = acceptId,
                      lastMatchEnd# = newLexerState
                    }
           in case off# lexerState >=# endOff# of
                1# -> case newMatch of
                  NoLastMatch# -> runOnEof newLexerState
                  SomeLastMatch# {lastMatchAccept#, lastMatchEnd#} ->
                    acceptSwitch lastMatchAccept# lastMatchEnd#
                _ ->
                  let !b = W8# (Exts.indexWord8Array# arr# (off# lexerState))
                      !stateId' = I32# (Exts.indexInt32OffAddr# stateTable (256# *# stateId +# unI# (fromIntegral @Word8 @Int b)))
                   in if stateId' == -1
                        then case newMatch of
                          NoLastMatch# -> runOnError newLexerState {off# = off# newLexerState +# 1#}
                          SomeLastMatch# {lastMatchAccept#, lastMatchEnd#} ->
                            acceptSwitch lastMatchAccept# lastMatchEnd#
                        else loop (unI# $ fromIntegral @Int32 @Int stateId') newLexerState {off# = off# newLexerState +# 1#} newMatch
      |]
  body <-
    [|
      withLexerState $ \lexerState ->
        loop
          $(TH.litE $ TH.intPrimL $ toInteger $ Dfa.start dfa)
          lexerState
          NoLastMatch#
      |]
  pure $ TH.LetE decs body

unI# :: Int -> Int#
unI# (I# i) = i
{-# INLINE unI# #-}
