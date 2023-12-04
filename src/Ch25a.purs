module Ch25a where

import Prelude
import Effect (Effect)
import Effect.Class.Console (log)
import Data.Generic.Rep (class Generic)
import Foreign.Generic (genericEncode, genericDecode, encodeJSON)
import Foreign.Generic.Class ( class Encode, class Decode, defaultOptions)

newtype Centimeters = Centimeters Number
newtype Kilograms = Kilograms Number
newtype Years = Years Int

type Personal =
  { height :: Centimeters
  , weight :: Kilograms
  , age :: Years
  }

newtype GPA = GPA Number

data Grade = Preschool | Kindergarten | Grade Int | High Int | College Int

type Student =
  { grade :: Grade
  , teacher :: Teacher
  , gpa :: GPA
  , personal :: Personal
  }

data TeachingStatus = Student | Probationary | NonTenured | Tenured

type Teacher =
  { grades :: Array Grade
  , numberOfStudents :: Int
  , personal :: Personal
  , status :: TeachingStatus
  }

newtype Centimeters = Centimeters Number
derive instance genericCentimeters :: Generic Centimeters _

instance encodeCentimeters :: Encode Centimeters where
  encode = genericEncode defaultOptions
  
instance decodeCentimeters :: Decode Centimeters where
  decode = genericDecode defaultOptions

test :: Effect Unit
test = do
  log $ encodeJSON $ Centimeters 12.34
