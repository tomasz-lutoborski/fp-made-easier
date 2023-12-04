module Ch8 where

import Data.List (List(..), (:))
import Data.NonEmpty (NonEmpty(..))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (log)

import Prelude (($))

class Semigroup a where
  append :: a -> a -> a

infixr 5 append as <>

class Semigroup m <= Monoid m where
  mempty :: m

class Monoid g <= Group g where
  ginverse :: g -> g

class Semiring a where
  add :: a -> a -> a
  zero :: a
  mul :: a -> a -> a
  one :: a

infixl 6 add as +
infixl 7 mul as *

combineNEListOfSemigroups :: forall a. Semigroup a => NonEmpty List a -> a
combineNEListOfSemigroups (NonEmpty x xs) = go x xs
  where
  go c Nil = c
  go c (y : ys) = go (c <> y) ys

test :: Effect Unit
test = do
  log $ show $ 20
