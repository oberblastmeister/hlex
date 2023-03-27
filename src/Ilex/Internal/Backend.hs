{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

module Ilex.Internal.Backend where

import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Vector qualified as VB
import Data.Word (Word8)
import GHC.Exts (Int#)
import Ilex.Internal.Monad
import Language.Haskell.TH qualified as TH

newtype LastMatch# = LastMatch# (# (# #) | (# Int#, LexerState# #) #)

pattern NoLastMatch# :: LastMatch#
pattern NoLastMatch# = LastMatch# (# (# #) | #)

pattern SomeLastMatch# :: Int# -> LexerState# -> LastMatch#
pattern SomeLastMatch# {lastMatchAccept#, lastMatchEnd#} = LastMatch# (# | (# lastMatchAccept#, lastMatchEnd# #) #)

{-# COMPLETE NoLastMatch#, SomeLastMatch# #-}

data BackendConfig = BackendConfig
  { acceptMap :: VB.Vector TH.ExpQ,
    onError :: TH.ExpQ,
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
    \(end :: LexerState#) -> withLexerEnv $ \LexerEnv {arr#, arrOff#} ->
      withLexerState $ \start -> do
        setLexerState end
        $exp
          ( LI
              LexerInput#
                { inputArr# = arr#,
                  inputArrOff# = arrOff#,
                  inputStart# = start,
                  inputEnd# = end
                }
          )
    |]

acceptSwitchExp :: VB.Vector TH.ExpQ -> TH.ExpQ
acceptSwitchExp acceptMap = do
  acceptIdName <- TH.newName "acceptId"
  bsName <- TH.newName "bs"
  arrOffName <- TH.newName "arrOff"
  startName <- TH.newName "start"
  endName <- TH.newName "end"
  let matches = flip map (zip [0 :: Int ..] $ VB.toList acceptMap) \(i, exp) -> do
        TH.match
          (TH.litP $ TH.intPrimL $ toInteger i)
          ( TH.normalB
              [|
                $exp
                  ( LI
                      LexerInput#
                        { inputArr# = $(TH.varE bsName),
                          inputArrOff# = $(TH.varE arrOffName),
                          inputStart# = $(TH.varE startName),
                          inputEnd# = $(TH.varE endName)
                        }
                  )
                |]
          )
          []
  let caseExpr =
        ( TH.caseE
            (TH.varE acceptIdName)
            (matches ++ [TH.match TH.wildP (TH.normalB [|error "invalid accept id"|]) []])
        )
  [|
    \($(TH.varP acceptIdName) :: Int#) ($(TH.varP endName) :: LexerState#) -> withLexerState $ \($(TH.varP startName)) ->
      withLexerEnv $ \LexerEnv {arr# = $(TH.varP bsName), arrOff# = $(TH.varP arrOffName)} -> do
        setLexerState $(TH.varE endName)
        $(caseExpr)
    |]
