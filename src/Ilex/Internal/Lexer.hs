{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Lexer where

import Control.Monad.Writer.CPS (MonadWriter, Writer)
import Control.Monad.Writer.CPS qualified as Writer
import Data.Monoid (Dual (..))
import Data.Primitive (ByteArray#)
import GHC.Exts (Int#)
import Ilex.Internal.Regex
import Language.Haskell.TH qualified as TH

type Lexer a = [Rule a]

newtype LexerBuilder a b = LexerBuilder
  { runLexerBuilder :: Writer (Dual [Rule a]) b
  }
  deriving
    (Functor, Applicative, Monad, MonadWriter (Dual [Rule a]))
    via (Writer (Dual [Rule a]))

data Rule a = Rule
  { regex :: Regex,
    accept :: a
  }

rule :: Regex -> TH.ExpQ -> LexerBuilder TH.ExpQ ()
rule regex accept = Writer.tell $ Dual [Rule {regex, accept = [|\(_ :: ByteArray#) (_ :: Int#) (_ :: Int#) -> $accept|]}]

evalLexerBuilder :: LexerBuilder a () -> [Rule a]
evalLexerBuilder = reverse . getDual . Writer.execWriter . runLexerBuilder
