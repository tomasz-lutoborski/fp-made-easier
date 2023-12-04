module Ch17
  ( Age(..)
  , FamilyAges(..)
  , Either(..)
  , FamilyAgesRow
  , Validation(..)
  , createFamilyAges
  , test
  ) where

import Prelude

import Data.Bifunctor (class Bifunctor)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)

data Maybe a = Nothing | Just a

derive instance genericMaybe :: Generic (Maybe a) _

instance showMaybe :: Show a => Show (Maybe a) where
  show = genericShow

instance functorMaybe :: Functor Maybe where
  map _ Nothing = Nothing
  map f (Just x) = Just $ f x

instance applyMaybe :: Apply Maybe where
  apply Nothing _ = Nothing
  apply (Just f) x = f <$> x

instance applicativeMaybe :: Applicative Maybe where
  pure = Just

data Either a b = Left a | Right b

derive instance eqEither :: (Eq a, Eq b) => Eq (Either a b)
derive instance ordEither :: (Ord a, Ord b) => Ord (Either a b)
derive instance genericEither :: Generic (Either a b) _
derive instance functorEither :: Functor (Either a)

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show = genericShow

instance bifunctorEither :: Bifunctor Either where
  bimap f _ (Left x) = Left $ f x
  bimap _ g (Right y) = Right $ g y

instance applyEither :: Apply (Either a) where
  apply (Left x) _ = Left x
  apply (Right f) x = f <$> x

instance applicativeEither :: Applicative (Either a) where
  pure = Right

newtype Validation err result = Validation (Either err result)

derive instance newtypeValidation :: Newtype (Validation err result) _
derive instance functorValidation :: Functor (Validation err)
derive instance bifunctorValidation :: Bifunctor Validation
derive instance genericValidation :: Generic (Validation err result) _
derive instance eqValidation :: (Eq err, Eq result) => Eq (Validation err result)
derive instance ordValidation :: (Ord err, Ord result) => Ord (Validation err result)

instance showValidation :: (Show err, Show result) => Show (Validation err result) where
  show = genericShow

instance applyValidation :: Semigroup err => Apply (Validation err) where
  apply (Validation (Left err1)) (Validation (Left err2)) = Validation $ Left (err1 <> err2)
  apply (Validation (Left err)) _ = Validation $ Left err
  apply (Validation (Right f)) x = f <$> x

instance applicativeValidation :: Semigroup err => Applicative (Validation err) where
  pure = Validation <<< Right

newtype Age = Age Int
newtype LowerAge = LowerAge Int
newtype UpperAge = UpperAge Int

derive instance genericAge :: Generic Age _
instance showAge :: Show Age where
  show = genericShow

validateAge :: LowerAge -> UpperAge -> Age -> String -> Validation (Array String) Age
validateAge (LowerAge lower) (UpperAge upper) (Age age) who
  | age < lower = Validation $ Left [ who <> "is too young" ]
  | age > upper = Validation $ Left [ who <> "is too old" ]
  | otherwise = Validation $ Right $ Age age

newtype Name = Name String

derive instance genericName :: Generic Name _
instance showName :: Show Name where
  show = genericShow

type FamilyAgesRow r = (fatherAge :: Age, motherAge :: Age, childAge :: Age | r)

type FamilyNamesRow r = (fatherName :: Name, motherName :: Name, childName :: Name | r)

newtype Family = Family { | FamilyNamesRow (FamilyAgesRow ()) }

derive instance genericFamily :: Generic Family _
instance showFamily :: Show Family where
  show = genericShow

newtype FamilyAges = FamilyAges { | FamilyAgesRow () }

derive instance genericFamilyAges :: Generic FamilyAges _
instance showFamilyAges :: Show FamilyAges where
  show = genericShow

createFamilyAges :: { | FamilyAgesRow () } -> Validation (Array String) FamilyAges
createFamilyAges { fatherAge, motherAge, childAge } =
  FamilyAges <$>
    ( { fatherAge: _, motherAge: _, childAge: _ }
        <$> validateAge (LowerAge 20) (UpperAge 40) fatherAge "Father "
        <*> validateAge (LowerAge 20) (UpperAge 40) motherAge "Mother "
        <*> validateAge (LowerAge 0) (UpperAge 18) childAge "Child "
    )

test :: Effect Unit
test = do
  log "placeholder"
  log $ show $ (+) <$> Just 21 <*> Just 21
  log $ show $ (*) <$> pure 2 <*> (pure 21 :: Maybe Int)
  log $ show $ pure (+) <*> Just 17 <*> Just 25
  -- LAW: Associative Composition
  -- (<<<) <$> u <*> v <*> w = u <*> (v <*> w)
  log $ show $ ((<<<) <$> pure identity <*> pure identity <*> pure 1) ==
    (pure identity <*> (pure identity <*> pure 1) :: Either Unit Int)
  -- LAW: Identity
  -- pure identity <*> x = x
  log $ show $ (pure identity <*> pure 1) == (pure 1 :: Either Unit Int)
  -- LAW: Homomorphism
  -- pure (f x) = pure f <*> pure x
  log $ show $ pure (negate 1) == (pure negate <*> pure 1 :: Either Unit Int)
  -- LAW: Interchange
  -- u <*> pure x = pure (_ $ x) <*> u
  log $ show $ (pure negate <*> pure 1) ==
    ( pure (_ $ 1) <*> pure negate
        :: Either Unit Int
    )
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 30, childAge: Age 10 }
  log $ show $ createFamilyAges
    { fatherAge: Age 400, motherAge: Age 300, childAge: Age 0 }
  log $ show $ createFamilyAges
    { fatherAge: Age 4
    , motherAge: Age 3
    , childAge: Age 10
    }
  log $ show $ createFamilyAges
    { fatherAge: Age 40, motherAge: Age 30, childAge: Age 100 }
  log $ show $ createFamilyAges
    { fatherAge: Age 40
    , motherAge: Age 3
    , childAge: Age 0
    }
