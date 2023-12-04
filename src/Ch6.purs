module Ch6 where

import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits as String
import Prelude (class Eq, ($), (&&), (==))

type Address =
  { street1 :: String
  , street2 :: String
  , city :: String
  , state :: String
  , zip :: String
  }

data Person = Person
  { name :: String
  , age :: Int
  , address :: Address
  }

data Company = Company
  { name :: String
  , address :: Address
  }

data Residence
  = Home Address
  | Facility Address

person :: Person
person = Person
  { name: "Joe Mama"
  , age: 22
  , address:
      { street1: "123 Main Street"
      , street2: "Apt 152"
      , city: "Jamestown"
      , state: "CA"
      , zip: "95327"
      }
  }

company :: Company
company = Company
  { name: "Acme"
  , address:
      { street1: "987 Tesla Way"
      , street2: "Suite 101"
      , city: "Irvine"
      , state: "CA"
      , zip: "92602"
      }
  }

home :: Residence
home = Home
  { street1: "1 1st Street"
  , street2: "Apt 1"
  , city: "Buford"
  , state: "WY"
  , zip: "82052"
  }

facility :: Residence
facility = Facility
  { street1: "54321 Countdown Ave"
  , street2: ""
  , city: "Huntsville"
  , state: "AL"
  , zip: "35805"
  }

class HasAddress a where
  getAddress :: a -> Address

instance hasAddressPerson :: HasAddress Person where
  getAddress (Person p) = p.address

instance hasAddressCompany :: HasAddress Company where
  getAddress (Company c) = c.address

instance hasAddressResidence :: HasAddress Residence where
  getAddress (Home h) = h
  getAddress (Facility f) = f

instance eqPerson :: Eq Person where
  eq (Person p1) (Person p2) = p1.name == p2.name && p1.age == p2.age && p1.address == p2.address

newtype Ceo = Ceo Person

derive instance newtypeCeo :: Newtype Ceo _
derive newtype instance hasAddressCeo :: HasAddress Ceo

newtype Janitor = Janitor Person

derive instance newtypeJanitor :: Newtype Janitor _
derive newtype instance hasAddressJanitor :: HasAddress Janitor

genericPersonHasAddress :: forall a. Newtype a Person => a -> Address
genericPersonHasAddress wrapped = getAddress $ unwrap wrapped

data Ordering = LT | EQ | GT

class Eq a <= Ord a where
  compare :: a -> a -> Ordering

instance eqOrdering :: Eq Ordering where
  eq LT LT = true
  eq EQ EQ = true
  eq GT GT = true
  eq _ _ = false

instance ordOrdering :: Ord Ordering where
  compare LT LT = EQ
  compare EQ EQ = EQ
  compare GT GT = EQ
  compare LT _ = LT
  compare EQ LT = GT
  compare EQ GT = LT
  compare GT _ = GT

data Place = First | Second | Third

instance eqPlace :: Eq Place where
  eq First First = true
  eq Second Second = true
  eq Third Third = true
  eq _ _ = false

class Decapitate collection element | element -> collection where
  decapitate :: collection -> Maybe { head :: element, tail :: collection }

instance decapitateString :: Decapitate String Char where
  decapitate = String.uncons
else instance decapitateList :: Decapitate (List a) a where
  decapitate = List.uncons
