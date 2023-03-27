{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

module Ilex.Internal.Backend.Fun where

import Data.Traversable (for)
import Data.Vector qualified as VB
import Data.Word (Word8)
import GHC.Exts (Int (..), (+#))
import GHC.Exts qualified as Exts
import GHC.Stack (HasCallStack)
import GHC.Word (Word8 (W8#))
import Ilex.Internal.Backend
import Ilex.Internal.Dfa (Dfa, Dfa' (Dfa))
import Ilex.Internal.Dfa qualified as Dfa
import Ilex.Internal.Monad
import Language.Haskell.TH qualified as TH

codegen :: BackendConfig -> Dfa Int -> TH.ExpQ
codegen config@BackendConfig {acceptMap} Dfa {Dfa.start, Dfa.states} = do
  stateToNameMap <- VB.generateM numStates (TH.newName . ("state" ++) . show)
  acceptSwitchName <- TH.newName "acceptSwitch"
  onErrorName <- TH.newName "onError"
  onEofName <- TH.newName "onEof"
  let codegenState name state = do
        let expanded = expandErrorStates $ Dfa.transitions state
        let bestDefault = bestDefaultTransition (snd <$> expanded)
        let expanded' = filter ((/= bestDefault) . snd) expanded
        lastMatchName <- TH.newName
          case Dfa.accept state of
            Just _ -> "_lastMatch"
            Nothing -> "lastMatch"
        lexerStateName <- TH.newName "lexerState"
        newLexerStateName <- TH.newName "newLexerState"
        newMatchName <- TH.newName "newMatch"
        let makeCombineLastMatch :: TH.ExpQ -> TH.ExpQ
            makeCombineLastMatch go =
              [|
                case $(TH.varE newMatchName) of
                  NoLastMatch# -> $(go)
                  SomeLastMatch# {lastMatchAccept#, lastMatchEnd#} -> do
                    $(TH.varE acceptSwitchName) lastMatchAccept# lastMatchEnd#
                |]
        let expanded'' = ((\(b, s) -> (bytePat b, s)) <$> expanded') ++ [(TH.WildP, bestDefault)]
        matches <-
          for expanded'' \(pat, s) -> do
            let nextState = stateToNameMap ! s
            goError <-
              makeCombineLastMatch
                [|
                  $(TH.varE onErrorName)
                    $(TH.varE newLexerStateName) {off# = off# $(TH.varE newLexerStateName) +# 1#}
                  |]
            let bodyExpr =
                  ( if s == -1
                      then pure goError
                      else do
                        [|
                          $(TH.varE nextState)
                            $(TH.varE newMatchName)
                            $(TH.varE newLexerStateName) {off# = off# $(TH.varE newLexerStateName) +# 1#}
                          |]
                  )
            TH.match
              (pure pat)
              (TH.normalB bodyExpr)
              []
        bVar <- TH.newName "b"
        let caseExp = TH.CaseE (TH.VarE bVar) matches
        let goEof = makeCombineLastMatch [|$(TH.varE onEofName) $(TH.varE newLexerStateName)|]
        let newLexerStateExp =
              if Dfa.isCharEnd state
                then [|$(TH.varE lexerStateName) {charOff# = charOff# $(TH.varE lexerStateName) +# 1#}|]
                else [|$(TH.varE lexerStateName)|]
        let newMatchExp = case Dfa.accept state of
              Nothing -> TH.varE lastMatchName
              Just acceptId ->
                let acceptIdExp = TH.litE (TH.intPrimL (toInteger acceptId))
                 in [|SomeLastMatch# $(acceptIdExp) $(TH.varE newLexerStateName)|]
        bodyExp <-
          [|
            \($(TH.varP lastMatchName) :: LastMatch#) ($(TH.varP lexerStateName) :: LexerState#) -> withLexerEnv $ \LexerEnv {arr#, endOff#} ->
              let !($(TH.varP newLexerStateName)) = $(newLexerStateExp)
                  !($(TH.varP newMatchName)) = $(newMatchExp)
               in if I# (off# $(TH.varE lexerStateName)) >= I# endOff#
                    then $goEof
                    else case W8# (Exts.indexWord8Array# arr# (off# $(TH.varE lexerStateName))) of
                      $(TH.varP bVar) -> $(pure caseExp)
            |]
        let clause = TH.FunD name [TH.Clause [] (TH.NormalB bodyExp) []]
        pure clause
  let stateDecs = flip map [0 .. numStates - 1] \s -> do
        let !name = stateToNameMap ! s
        let !state = states ! s
        codegenState name state
      onErrorExp = withInpExp (onError config)
      onEofExp = withInpExp (onEof config)
      onErrorDec = TH.valD (TH.varP onErrorName) (TH.normalB onErrorExp) []
      onEofDec = TH.valD (TH.varP onEofName) (TH.normalB onEofExp) []
      decs = onErrorDec : onEofDec : acceptSwitchDec acceptMap acceptSwitchName : stateDecs
  TH.letE decs [|withLexerState $ $(TH.varE (stateToNameMap ! start)) NoLastMatch#|]
  where
    numStates = VB.length states

(!) :: HasCallStack => VB.Vector a -> Int -> a
v ! i
  | i < 0 || i >= VB.length v = error $ "index " ++ show i ++ " is out of bounds"
  | otherwise = v VB.! i

bytePat :: Word8 -> TH.Pat
bytePat b = TH.SigP (TH.LitP $ TH.IntegerL $ fromIntegral b) (TH.ConT ''Word8)
