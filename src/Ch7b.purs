module Ch7b where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Console (log)

newtype Age = Age Int

derive instance newtypeAge :: Newtype Age _
derive newtype instance showAge :: Show Age
derive newtype instance eqAge :: Eq Age

newtype FullName = FullName String

derive instance newtypeFullName :: Newtype FullName _
derive newtype instance eqFullName :: Eq FullName

instance showFullName :: Show FullName where
  show (FullName name) = name

data Occupation = Doctor | Dentist | Lawyer | Unemployed

instance showOccupation :: Show Occupation where
  show Doctor = "Doctor"
  show Dentist = "Dentist"
  show Lawyer = "Lawyer"
  show Unemployed = "Unemployed"

toOccupation :: String -> Maybe Occupation
toOccupation = case _ of
  "Doctor" -> Just Doctor
  "Dentist" -> Just Dentist
  "Lawyer" -> Just Lawyer
  "Unemployed" -> Just Unemployed
  _ -> Nothing

derive instance genericOccupation :: Generic Occupation _
derive instance eqOccupation :: Eq Occupation

data Person = Person
  { name :: FullName
  , age :: Age
  , occupation :: Occupation
  }

derive instance genericPerson :: Generic Person _
derive instance eqPerson :: Eq Person

instance toCSVPerson :: ToCSV Person where
  toCSV (Person { name, age, occupation }) =
    CSV $ show name <> "," <> show age <> "," <> show occupation

instance fromCSVPerson :: FromCSV Person where
  fromCSV (CSV str) = case split (Pattern ",") str of
    [ name, age, occupation ] -> case fromString age of
      Just age' -> case toOccupation occupation of
        Just occupation' -> Just $ Person
          { name: FullName name
          , age: Age age'
          , occupation: occupation'
          }
        Nothing -> Nothing
      Nothing -> Nothing
    _ -> Nothing

newtype CSV = CSV String

derive instance newtypeCSV :: Newtype CSV _
derive newtype instance eqCSV :: Eq CSV
derive newtype instance showCSV :: Show CSV

class ToCSV a where
  toCSV :: a -> CSV

class FromCSV a where
  fromCSV :: CSV -> Maybe a

test :: Effect Unit
test = do
  let
    csvComparison =
      toCSV
        ( Person
            { name: FullName "Sue Smith"
            , age: Age 23
            , occupation: Doctor
            }
        ) == CSV "Sue Smith,23,Doctor"
  log $ show csvComparison
  let
    person = Person
      { name: FullName "Sue Smith"
      , age: Age 23
      , occupation: Doctor
      }
  let roundTrip = (toCSV person # fromCSV) == Just person
  log $ show roundTrip
  log $ show $ toCSV person
