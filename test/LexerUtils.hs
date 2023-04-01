module LexerUtils where

import Hlex
import Hlex.Internal.Regex qualified as R

data Span = Span
  { start :: {-# UNPACK #-} !Pos,
    end :: {-# UNPACK #-} !Pos
  }
  deriving (Show, Eq)

data Spanned a = Spanned
  { span :: !Span,
    value :: a
  }
  deriving (Show, Eq)

lexUntil :: (t -> Bool) -> Lex s t -> Lex s [t]
lexUntil p lex = do
  t <- lex
  if p t
    then pure [t]
    else do
      ts <- lexUntil p lex
      pure $ t : ts

-- spanned :: (LexerInput -> a) -> LexerInput -> Lex s (Spanned a)
-- spanned f i = do
--   let start = inputStart i
--   let end = inputEnd i
--   pure $ Spanned (Span start end) $ f i

-- tok :: a -> LexerInput -> Lex s (Spanned a)
-- tok = spanned . const

rLower :: R.Regex
rLower = R.range ('a', 'z')

rUpper :: R.Regex
rUpper = R.range ('A', 'Z')

rIdent :: R.Regex
rIdent = R.alt [R.alpha, R.digit, "_", "'"]
