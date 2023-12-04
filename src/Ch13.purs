module Ch13 where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String.Common (toUpper)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, identity, show, ($), (*), (/), (<<<), (<>), (==))

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _
derive instance eqMaybe :: Eq a => Eq (Maybe a)
instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

data Either a b = Left a | Right b

derive instance genericEither :: Generic (Either a b) _
instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

data Tuple a b = Tuple a b

derive instance genericTuple :: Generic (Tuple a b) _
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show = genericShow

class Functor f where
  map :: ∀ a b. (a -> b) -> f a -> f b

infixl 4 map as <$>

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just a) = Just $ f a

instance functorEither :: Functor (Either a) where
  map _ (Left a) = Left a
  map f (Right b) = Right $ f b

instance functorTuple :: Functor (Tuple a) where
  map f (Tuple a b) = Tuple a $ f b

class Bifunctor f where
  bimap :: ∀ a b c d. (a -> c) -> (b -> d) -> f a b -> f c d

rmap :: ∀ f a b c. Bifunctor f => (b -> c) -> f a b -> f a c
rmap = bimap identity

lmap :: ∀ f a b c. Bifunctor f => (a -> c) -> f a b -> f c b
lmap f = bimap f identity

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left a) = Left $ f a
  bimap _ g (Right b) = Right $ g b

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple a b) = Tuple (f a) (g b)

test :: Effect Unit
test = do
  log $ show $ (_ / 2) <$> Just 10
  log $ show $ (_ / 2) <$> Nothing
  log $ show $ (_ / 2) <$> (Right 10 :: Either Unit _)
  log $ show $ (_ / 2) <$> Left "error reason"
  log $ show $ (_ / 2) <$> Tuple 10 20
  log $ show $ "Maybe Identity for Nothing: "
    <> show ((identity <$> Nothing) == (Nothing :: Maybe Unit))
  let
    g x = x * 2
    f x = x * 3
  log $ show $ "Maybe composition for Nothing: "
    <> show (map (g <<< f) Nothing == (map g <<< map f) Nothing)
  log $ show $ "Maybe composition for Just: "
    <> show (map (g <<< f) (Just 10) == (map g <<< map f) (Just 10))
  log $ show $ rmap (_ * 2) $ Left "error reason"
  log $ show $ rmap (_ * 2) $ (Right 10 :: Either Unit _)
  log $ show $ lmap toUpper $ (Left "error reason" :: Either _ Unit)
  log $ show $ lmap toUpper $ Right 10
  log $ show $ rmap (_ * 2) $ Tuple 80 40
  log $ show $ lmap (_ / 2) $ Tuple 80 40
  log $ show $ bimap (_ / 2) (_ * 2) $ Tuple 80 40
