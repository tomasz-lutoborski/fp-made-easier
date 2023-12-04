module Ch15 where

import Prelude

import Data.Foldable (class Foldable, foldl)
import Data.Functor.Contravariant (class Contravariant, (>$<))
import Data.Int.Bits ((.&.))
import Data.List (List(..), (:))
import Data.Profunctor (class Profunctor, dimap)
import Data.String (length)
import Effect (Effect)
import Effect.Console (log)

even :: Int -> Boolean
even x = x .&. 1 == 0

odd :: Int -> Boolean
odd x = x .&. 1 == 1

data Predicate a = Predicate (a -> Boolean)

runPredicate :: ∀ a. Predicate a -> a -> Boolean
runPredicate (Predicate f) = f

instance contravariantPredicate :: Contravariant Predicate where
  cmap f (Predicate g) = Predicate (g <<< f)

data Moore s a b = Moore s (s -> b) (s -> a -> s)

instance profunctorMoore :: Profunctor (Moore s) where
  dimap :: ∀ a b c d. (c -> a) -> (b -> d) -> Moore s a b -> Moore s c d
  dimap f g (Moore s0 output transition) =
    Moore s0 (g <<< output) (\s -> transition s <<< f)

addr :: ∀ a. Semiring a => Moore a a a
addr = Moore zero identity (+)

runFoldL :: ∀ a b f s. Foldable f => Moore s a b -> f a -> b
runFoldL (Moore s0 output transition) = output <<< foldl transition s0

sizer :: Moore Int String String
sizer = dimap length (\n -> "Size: " <> show n) addr

test :: Effect Unit
test = do
  log $ show $ odd 0
  log $ show $ odd 1
  log "------------------------------------"
  log $ show $ runPredicate (Predicate odd) $ 10
  log $ show $ runPredicate (Predicate odd) $ 11
  log $ show $ runPredicate ((_ + 1) >$< (Predicate odd)) 10
  log $ show $ runPredicate ((_ + 2) >$< (Predicate odd)) 10
  log "------------------------------------"
  log $ show $ runFoldL addr [ 1, 2, 3, 4, 5 ]
  log $ show $ runFoldL addr (1.0 : 2.0 : 3.0 : 4.0 : 5.0 : Nil)
  log $ show $ runFoldL sizer [ "This", "is", "the", "test" ]
