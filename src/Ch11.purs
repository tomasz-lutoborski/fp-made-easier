module Ch11 where

import Data.Foldable (class Foldable, foldMap, foldr)
import Data.List (List(..), foldl, singleton, (:))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty(..))
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Ord, class Semiring, type (~>), Unit, discard, negate, otherwise, show, zero, ($), (+), (<<<), (<>), (>))

reverse :: List ~> List
reverse =
  foldl (\acc x -> x : acc) Nil

max :: ∀ a. Ord a => a -> a -> a
max x y
  | x > y = x
  | otherwise = y

findMax :: ∀ a. Ord a => a -> List a -> a
findMax m Nil = m
findMax m (x : xs) = findMax (max m x) xs

findMax' :: ∀ a. Ord a => List a -> Maybe a
findMax' Nil = Nothing
findMax' l@(first : _) = Just $ go first l
  where
  go :: a -> List a -> a
  go m Nil = m
  go m (x : xs) = go (max m x) xs

findMaxFold :: ∀ a. Ord a => List a -> Maybe a
findMaxFold Nil = Nothing
findMaxFold l@(first : _) = Just $ foldl max first l

findMaxNE :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE (NonEmptyList (NonEmpty first l)) = foldl max first l

findMaxNE' :: ∀ a. Ord a => NonEmptyList a -> a
findMaxNE' (NonEmptyList ne) = foldl1 max ne

foldl1 :: ∀ f a. Foldable f => (a -> a -> a) -> NonEmpty f a -> a
foldl1 f (NonEmpty first l) = foldl f first l

sum :: List Int -> Int
sum Nil = 0
sum (x : xs) = x + sum xs

sum' :: List Int -> Int
sum' l = go 0 l
  where
  go :: Int -> List Int -> Int
  go acc Nil = acc
  go acc (x : xs) = go (acc + x) xs

sum'' :: ∀ f a. Semiring a => Foldable f => f a -> a
sum'' = foldl (+) zero

data Tree a = Leaf a | Node (Tree a) (Tree a)

toList :: ∀ a. Tree a -> List a
toList (Leaf x) = singleton x
toList (Node l r) = toList l <> toList r

instance foldableTree :: Foldable Tree where
  foldl f acc = foldl f acc <<< toList
  foldr f acc = foldr f acc <<< toList
  foldMap f = foldMap f <<< toList

test :: Effect Unit
test = do
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ max (-1) 99
  log $ show $ max "aa" "z"
  log $ show $ findMax 0 (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax "" ("a" : "bbb" : "c" : Nil)
  log $ show $ findMax' (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMax' ("a" : "bbb" : "c" : Nil)
  log $ show $ findMaxFold (37 : 311 : -1 : 2 : 84 : Nil)
  log $ show $ findMaxFold ("a" : "bbb" : "c" : Nil)
  log $ show $ sum (1 : 2 : 3 : Nil)
