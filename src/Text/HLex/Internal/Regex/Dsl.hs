module Text.HLex.Internal.Regex.Dsl
  ( Regex,
    -- runRegex,
    empty,
    lit,
    -- range,
    opt,
    some,
    many,
    allOf,
    anyOf,
    (~|),
    (~&),
  )
where

import Control.Monad (ap)
import Data.Text (Text)
import Text.HLex.Internal.Range (Range)
import Text.HLex.Internal.Range qualified as Range
import Text.HLex.Internal.Regex.Core qualified as Core

instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free as) = Free $ fmap f <$> as

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  Pure x >>= f = f x
  Free g >>= f = Free ((>>= f) <$> g)

data Free f a = Pure a | Free (f (Free f a))

data RegexF a
  = Empty a
  | Lit !Text a
  | Range !Char !Char a
  | Some (Regex ()) a
  | Many (Regex ()) a
  | Opt (Regex ()) a
  | AllOf (Regex ()) a
  | AnyOf (Regex ()) a
  deriving (Functor)

type Regex = Free RegexF

type f ~> g = forall a. f a -> g a

infixr 0 ~>

liftF :: Functor f => f ~> Free f
liftF f = Free $ Pure <$> f

-- runRegex :: Regex () -> Core.Regex
-- runRegex = runRegexConcat

-- runRegexConcat :: Regex () -> Core.Regex
-- runRegexConcat = runRegexWith Core.Concat

-- runRegexAlt :: Regex () -> Core.Regex
-- runRegexAlt = runRegexWith Core.Alt

-- runRegexWith :: (Core.Regex -> Core.Regex -> Core.Regex) -> Regex () -> Core.Regex
-- runRegexWith f = foldr f Core.Empty . reverse . runRegex'

-- runRegex' :: Regex () -> [Core.Regex]
-- runRegex' = go []
--   where
--     go rs (Pure ()) = rs
--     go rs (Free f) = case f of
--       Empty k -> go rs k
--       Lit s k -> next (Core.text s) k
--       Range r k -> next (Core.charRange r) k
--       Some m k -> let !re' = runRegexConcat m in next re' k
--       Many m k -> let !re' = runRegexConcat m in next re' k
--       Opt m k -> let !re' = runRegexConcat m in next (Core.zeroOrOne re') k
--       AllOf m k -> let !re' = runRegexConcat m in next re' k
--       AnyOf m k -> let !re' = runRegexAlt m in next re' k
--       where
--         next !x = go (x : rs)

empty :: Regex ()
empty = liftF $ Empty ()

lit :: Text -> Regex ()
lit s = liftF $ Lit s ()

-- range :: Char -> Char -> Regex ()
-- range c1 c2 = liftF $ Range (Range.new c1 c2) ()

opt :: Regex () -> Regex ()
opt r = liftF $ Opt r ()

some :: Regex () -> Regex ()
some r = liftF $ Some r ()

many :: Regex () -> Regex ()
many r = liftF $ Many r ()

allOf :: Regex () -> Regex ()
allOf r = liftF $ AllOf r ()

anyOf :: Regex () -> Regex ()
anyOf r = liftF $ AnyOf r ()

(~|) :: Regex () -> Regex () -> Regex ()
re1 ~| re2 = anyOf $ do re1; re2

(~&) :: Regex () -> Regex () -> Regex ()
re1 ~& re2 = allOf $ do re1; re2
