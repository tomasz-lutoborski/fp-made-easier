module Ch7a where

import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Ord (class Ord, Ordering(..), compare)
import Data.Show (class Show)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, discard, show, ($), (<), (<=), (<>), (==), (>), (>=), (||))

data Maybe a = Nothing | Just a

data Either a b = Left a | Right b

derive instance eqMaybe :: Eq a => Eq (Maybe a)

derive instance ordMaybe :: Ord a => Ord (Maybe a)

derive instance genericMaybe :: Generic (Maybe a) _

greaterThanOrEqual :: forall a. Ord a => a -> a -> Boolean
greaterThanOrEqual x y = cmp == EQ || cmp == GT
  where
  cmp = compare x y

instance showMaybe :: Show a => Show (Maybe a) where
  show Nothing = "Nothing"
  show (Just x) = "Just " <> show x

test :: Effect Unit
test = do
  log $ show $ Just 5 == Just 5
  log $ show $ Just 5 == Just 2
  log $ show $ Just 5 == Nothing
  log $ show $ Nothing == Just 5
  log $ show $ Nothing == (Nothing :: Maybe Unit)
  log "------------------"
  log $ show $ Just 1 < Just 5
  log $ show $ Just 5 <= Just 5
  log $ show $ Just 5 > Just 10
  log $ show $ Just 10 >= Just 10
  log $ show $ Just 99 > Nothing
  log $ show $ Just 99 < Nothing
