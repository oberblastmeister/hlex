{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

module Hlex.Internal.Backend
  ( withInpExp,
    acceptSwitchExp,
    acceptSwitchDec,
    bestDefaultTransition,
    expandErrorStates,
    LastMatch# (SomeLastMatch#, NoLastMatch#, lastMatchAccept#, lastMatchEnd#),
    MaybeRunId# (SomeRunId#, NoRunId#),
    BackendConfig (..),
  )
where

import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Vector qualified as VB
import Data.Word (Word8)
import GHC.Exts (Int#)
import Hlex.Internal.Monad
import Language.Haskell.TH qualified as TH

newtype MaybeRunId# = MaybeRunId# (# (# #) | (# Int# #) #)

pattern NoRunId# :: MaybeRunId#
pattern NoRunId# = MaybeRunId# (# (# #) | #)

pattern SomeRunId# :: Int# -> MaybeRunId#
pattern SomeRunId# runId = MaybeRunId# (# | (# runId #) #)

{-# COMPLETE NoRunId#, SomeRunId# #-}

newtype LastMatch# = LastMatch# (# (# #) | (# Int#, Pos# #) #)

pattern NoLastMatch# :: LastMatch#
pattern NoLastMatch# = LastMatch# (# (# #) | #)

pattern SomeLastMatch# :: Int# -> Pos# -> LastMatch#
pattern SomeLastMatch# {lastMatchAccept#, lastMatchEnd#} = LastMatch# (# | (# lastMatchAccept#, lastMatchEnd# #) #)

{-# COMPLETE NoLastMatch#, SomeLastMatch# #-}

data BackendConfig = BackendConfig
  { onInvalidUtf8 :: Maybe TH.ExpQ,
    onEof :: TH.ExpQ
  }

expandErrorStates :: IntMap Int -> [(Word8, Int)]
expandErrorStates m = [(b, Maybe.fromMaybe -1 $ IntMap.lookup (fromIntegral @Word8 @Int b) m) | b <- [0 :: Word8 .. 255]]

bestDefaultTransition :: [Int] -> Int
bestDefaultTransition l = fst $ List.maximumBy (compare `on` snd) stateWithOccurrences
  where
    stateWithOccurrences = (\ne -> (NE.head ne, length ne)) <$> NE.group (List.sort l)

acceptSwitchDec :: VB.Vector TH.ExpQ -> TH.Name -> TH.DecQ
acceptSwitchDec acceptMap name =
  TH.valD
    (TH.varP name)
    (TH.normalB $ acceptSwitchExp acceptMap)
    []

withInpExp :: TH.ExpQ -> TH.ExpQ
withInpExp exp = do
  [|
    \(end :: Pos#) -> withInput# $ \(input :: Input# u) ->
      withPos# $ \start -> do
        setPos# end
        $exp
          ( liftInput# @u
              input
                { inputStart# = off# start,
                  inputEnd# = off# end
                }
          )
    |]

acceptSwitchExp :: VB.Vector TH.ExpQ -> TH.ExpQ
acceptSwitchExp acceptMap = do
  acceptIdName <- TH.newName "acceptId"
  startName <- TH.newName "start"
  endName <- TH.newName "end"
  inputName <- TH.newName "input"
  uName <- TH.newName "u"
  let matches = flip map (zip [0 :: Int ..] $ VB.toList acceptMap) \(i, exp) -> do
        TH.match
          (TH.litP $ TH.intPrimL $ toInteger i)
          ( TH.normalB
              [|
                $exp
                  ( liftInput# @($(TH.varT uName))
                      $(TH.varE inputName)
                        { inputStart# = off# $(TH.varE startName),
                          inputEnd# = off# $(TH.varE endName)
                        }
                  )
                |]
          )
          []
  let caseExpr =
        ( TH.caseE
            (TH.varE acceptIdName)
            matches
        )
  [|
    \($(TH.varP acceptIdName) :: Int#) ($(TH.varP endName) :: Pos#) -> withPos# $ \($(TH.varP startName)) ->
      withInput# $ \($(TH.varP inputName) :: Input# $(TH.varT uName)) -> do
        setPos# $(TH.varE endName)
        $(caseExpr)
    |]
