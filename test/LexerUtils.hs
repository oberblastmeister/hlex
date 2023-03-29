module LexerUtils where

import Ilex
import Ilex.Internal.Regex qualified as R

data Span = Span
  { start :: !Pos,
    end :: !Pos
  }
  deriving (Show, Eq)

data Spanned a = Spanned
  { span :: !Span,
    value :: a
  }
  deriving (Show, Eq)

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
