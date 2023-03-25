{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Text.HLex.Internal.Codegen where

import Control.Monad.State (MonadState (..))
import Data.Function (on)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe qualified as Maybe
import Data.Primitive (ByteArray#)
import Data.Traversable (for)
import Data.Vector qualified as VB
import Data.Word (Word8)
import GHC.Exts (Int (..), Int#, (+#))
import GHC.Exts qualified as Exts
import Language.Haskell.TH qualified as TH
import Text.HLex.Internal.Dfa (Dfa, Dfa' (Dfa))
import Text.HLex.Internal.Dfa qualified as Dfa

newtype LexerEnv = LexerEnv# (# ByteArray#, Int#, Int# #)

pattern LexerEnv :: ByteArray# -> Int# -> Int# -> LexerEnv
pattern LexerEnv {arr, len, arrOff} = LexerEnv# (# arr, len, arrOff #)

{-# COMPLETE LexerEnv #-}

newtype LexerState = LexerState# (# Int# #)

pattern LexerState :: Int# -> LexerState
pattern LexerState {off} = LexerState# (# off #)

{-# COMPLETE LexerState #-}

newtype LineCol# = LineCol## (# Int#, Int# #)

pattern LineCol# :: Int# -> Int# -> LineCol#
pattern LineCol# {line#, col#} = LineCol## (# line#, col# #)

{-# COMPLETE LineCol# #-}

data LineCol = LineCol
  { line :: !Int,
    col :: !Int
  }

liftLineCol :: LineCol# -> LineCol
liftLineCol (LineCol# {line#, col#}) = LineCol {line = I# line#, col = I# col#}
{-# INLINE liftLineCol #-}

class Monad m => MonadLexer m where
  withLexerEnv :: (LexerEnv -> m a) -> m a
  withLexerState :: (LexerState -> m a) -> m a
  setLexerState :: LexerState -> m ()

newtype Lexer s a = Lexer {runLexer :: LexerEnv -> LexerState -> s -> (# LexerState, s, a #)}

instance Functor (Lexer s) where
  fmap f (Lexer m) = Lexer \env ls s -> case m env ls s of
    (# ls, s, a #) -> (# ls, s, f a #)
  {-# INLINE fmap #-}

instance Applicative (Lexer s) where
  pure a = Lexer \_ ls s -> (# ls, s, a #)
  {-# INLINE pure #-}
  Lexer f <*> Lexer a = Lexer \env ls s -> case f env ls s of
    (# ls, s, f' #) -> case a env ls s of
      (# ls, s, a' #) -> (# ls, s, f' a' #)
  {-# INLINE (<*>) #-}

instance Monad (Lexer s) where
  Lexer m >>= f = Lexer \env ls s -> case m env ls s of
    (# ls, s, a #) -> runLexer (f a) env ls s
  {-# INLINE (>>=) #-}

instance MonadState s (Lexer s) where
  get = Lexer \_ ls s -> (# ls, s, s #)
  {-# INLINE get #-}
  put s = Lexer \_ ls _ -> (# ls, s, () #)
  {-# INLINE put #-}

instance MonadLexer (Lexer s) where
  withLexerEnv f = Lexer \env ls s -> runLexer (f env) env ls s
  {-# INLINE withLexerEnv #-}
  withLexerState f = Lexer \env ls s -> runLexer (f ls) env ls s
  {-# INLINE withLexerState #-}
  setLexerState ls = Lexer \_env _ s -> (# ls, s, () #)
  {-# INLINE setLexerState #-}

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
codegen CodegenConfig {acceptMap, onEof, onError} Dfa {Dfa.start, Dfa.states} = do
  stateToNameMap <- VB.generateM numStates (TH.newName . ("state" ++) . show)
  acceptSwitchName <- TH.newName "acceptSwitch"
  let codegenState name state = do
        let expanded = expandErrorStates $ Dfa.transitions state
        let bestDefault = bestDefaultTransition (snd <$> expanded)
        let expanded' = filter ((/= bestDefault) . snd) expanded
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
        matches <-
          for expanded' \(b, s) -> do
            let nextState = stateToNameMap VB.! s
            goError <- makeCombineLastMatch onError
            TH.match
              (pure $ bytePat b)
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
        let finalMatch =
              TH.Match
                TH.WildP
                (TH.NormalB (TH.VarE (stateToNameMap VB.! bestDefault)))
                []
        bVar <- TH.newName "b"
        let caseExp = TH.CaseE (TH.VarE bVar) (matches ++ [finalMatch])
        let goEof = makeCombineLastMatch onEof
        bodyExp <-
          [|
            \($(TH.varP lastMatchName)) ($(TH.varP posName)) -> withLexerEnv $ \LexerEnv {arr, len} ->
              if I# $(TH.varE posName) >= I# len
                then $goEof
                else case Exts.indexWord8Array# arr $(TH.varE posName) of
                  $(TH.varP bVar) -> $(pure caseExp)
            |]
        let clause = TH.FunD name [TH.Clause [] (TH.NormalB bodyExp) []]
        pure clause
  let acceptSwitchDec =
        TH.valD
          (TH.varP acceptSwitchName)
          (TH.normalB $ generateAcceptSwitch acceptMap)
          []
      stateDecs = flip map [1 .. numStates] \s -> do
        let name = stateToNameMap VB.! s
        let state = states VB.! s
        codegenState name state
      decs = acceptSwitchDec : stateDecs
  TH.letE decs (pure $ TH.VarE (stateToNameMap VB.! start))
  where
    numStates = VB.length states

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
    [TH.varP acceptIdName, TH.varP bsName, TH.varP endName]
    [|
      withLexerState $ \LexerState {off} -> do
        let $(TH.varP startName) = off
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
