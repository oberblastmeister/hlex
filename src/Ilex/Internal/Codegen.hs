{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

module Ilex.Internal.Codegen where

import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Traversable (for)
import Data.Vector qualified as VB
import Data.Word (Word8)
import Debug.Trace
import GHC.Exts (Int (..), Int#, (+#))
import GHC.Exts qualified as Exts
import GHC.Stack (HasCallStack)
import GHC.Word (Word8 (W8#))
import Ilex.Internal.Dfa (Dfa, Dfa' (Dfa))
import Ilex.Internal.Dfa qualified as Dfa
import Ilex.Internal.Monad
import Language.Haskell.TH qualified as TH

type LastMatch# = (# (# #) | (# Int#, Int# #) #)

pattern NoLastMatch# :: LastMatch#
pattern NoLastMatch# = (# (# #) | #)

pattern SomeLastMatch# :: Int# -> Int# -> LastMatch#
pattern SomeLastMatch# {lastMatchAccept#, lastMatchEnd#} = (# | (# lastMatchAccept#, lastMatchEnd# #) #)

{-# COMPLETE NoLastMatch#, SomeLastMatch# #-}

data CodegenConfig = CodegenConfig
  { acceptMap :: VB.Vector TH.ExpQ,
    onError :: TH.ExpQ,
    onEof :: TH.ExpQ
  }

codegen :: CodegenConfig -> Dfa Int -> TH.ExpQ
codegen config@CodegenConfig {acceptMap} Dfa {Dfa.start, Dfa.states} = do
  stateToNameMap <- VB.generateM numStates (TH.newName . ("state" ++) . show)
  let !_ = traceId $ "stateToNameMap " ++ show stateToNameMap
  acceptSwitchName <- TH.newName "acceptSwitch"
  onErrorName <- TH.newName "onError"
  onEofName <- TH.newName "onEof"
  let codegenState name state = do
        let expanded = expandErrorStates $ Dfa.transitions state
        let bestDefault = bestDefaultTransition (snd <$> expanded)
        let expanded' = filter ((/= bestDefault) . snd) expanded
        let !_ = traceId $ show bestDefault
        let !_ = traceId $ show expanded'
        lastMatchName <- TH.newName "lastMatch"
        posName <- TH.newName "pos"
        let makeCombineLastMatch :: TH.ExpQ -> TH.ExpQ
            makeCombineLastMatch go =
              [|
                case $(TH.varE lastMatchName) of
                  NoLastMatch# -> $(go)
                  SomeLastMatch# {lastMatchAccept#, lastMatchEnd#} -> do
                    -- withLexerState $ \ls -> setLexerState ls {off = lastMatchEnd#}
                    $(TH.varE acceptSwitchName) lastMatchAccept# lastMatchEnd#
                |]
        let expanded'' = ((\(b, s) -> (bytePat b, s)) <$> expanded') ++ [(TH.WildP, bestDefault)]
        matches <-
          for expanded'' \(pat, s) -> do
            let !_ = traceId $ "s: " ++ show s ++ " "
            let nextState = stateToNameMap ! s
            goError <- makeCombineLastMatch (TH.varE onErrorName)
            TH.match
              (pure pat)
              ( TH.normalB
                  ( if s == -1
                      then pure goError
                      else do
                        let newMatch = case Dfa.accept state of
                              Nothing -> TH.varE lastMatchName
                              Just acceptId ->
                                let acceptIdExp = TH.litE (TH.intPrimL (toInteger acceptId))
                                 in [|SomeLastMatch# $(acceptIdExp) ($(TH.varE posName) +# 1#)|]
                        [|$(TH.varE nextState) $(newMatch) ($(TH.varE posName) +# 1#)|]
                  )
              )
              []
        bVar <- TH.newName "b"
        let caseExp = TH.CaseE (TH.VarE bVar) matches
        let goEof = makeCombineLastMatch (TH.varE onEofName)
        bodyExp <-
          [|
            \($(TH.varP lastMatchName)) ($(TH.varP posName)) -> withLexerEnv $ \LexerEnv {arr, len} ->
              if I# $(TH.varE posName) >= I# len
                then $goEof
                else case W8# (Exts.indexWord8Array# arr $(TH.varE posName)) of
                  $(TH.varP bVar) -> $(pure caseExp)
            |]
        let clause = TH.FunD name [TH.Clause [] (TH.NormalB bodyExp) []]
        pure clause
  let acceptSwitchDec =
        TH.valD
          (TH.varP acceptSwitchName)
          (TH.normalB $ generateAcceptSwitch acceptMap)
          []
      !_ = traceId $ "numStates': " ++ show numStates
      stateDecs = flip map [0 .. numStates - 1] \s -> do
        let !name = stateToNameMap ! s
        let !state = states ! s
        codegenState name state
      onErrorDec = TH.valD (TH.varP onErrorName) (TH.normalB (onError config)) []
      onEofDec = TH.valD (TH.varP onEofName) (TH.normalB (onEof config)) []
      decs = onErrorDec : onEofDec : acceptSwitchDec : stateDecs
  TH.letE decs [|$(TH.varE (stateToNameMap ! start)) NoLastMatch# 0#|]
  where
    !_ = traceId $ "numStates: " ++ show numStates
    numStates = VB.length states

(!) :: HasCallStack => VB.Vector a -> Int -> a
(!) v i
  | i < 0 || i >= VB.length v = error $ "index " ++ show i ++ " is out of bounds"
  | otherwise = v VB.! i

generateAcceptSwitch :: VB.Vector TH.ExpQ -> TH.ExpQ
generateAcceptSwitch acceptMap = do
  acceptIdName <- TH.newName "acceptId"
  bsName <- TH.newName "bs"
  startName <- TH.newName "start"
  endName <- TH.newName "end"
  let matches = flip map (zip [0 :: Int ..] $ VB.toList acceptMap) \(i, exp) -> do
        TH.match
          (TH.litP $ TH.intPrimL $ toInteger i)
          (TH.normalB (TH.appsE [exp, TH.varE bsName, TH.varE startName, TH.varE endName]))
          []
  let caseExpr =
        ( TH.caseE
            (TH.varE acceptIdName)
            matches
        )
  TH.lamE
    [TH.varP acceptIdName, TH.varP endName]
    [|
      withLexerState $ \LexerState {off = $(TH.varP startName)} ->
        withLexerEnv $ \LexerEnv {arr = $(TH.varP bsName)} -> do
          setLexerState LexerState {off = $(TH.varE endName)}
          $(caseExpr)
      |]

bytePat :: Word8 -> TH.Pat
bytePat b = TH.SigP (TH.LitP $ TH.IntegerL $ fromIntegral b) (TH.ConT ''Word8)

expandErrorStates :: IntMap Int -> [(Word8, Int)]
expandErrorStates m = [(b, Maybe.fromMaybe -1 $ IntMap.lookup (fromIntegral @Word8 @Int b) m) | b <- [1 :: Word8 .. 255]]

bestDefaultTransition :: [Int] -> Int
bestDefaultTransition l = fst $ List.maximumBy (compare `on` snd) stateWithOccurrences
  where
    stateWithOccurrences = (\ne -> (NE.head ne, length ne)) <$> NE.group (List.sort l)
