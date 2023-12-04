module Ch5 where

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)
import Prelude (type (~>), Unit, discard, negate, otherwise, show, (+), (-), (<), (==), (>>>))

flip :: forall a b c. (a -> b -> c) -> b -> a -> c
flip f x y = f y x

const :: forall a b. a -> b -> a
const x _ = x

apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped x f = f x

infixl 1 applyFlipped as #

singleton :: forall a. a -> List a
singleton x = x : Nil

null :: forall a. List a -> Boolean
null Nil = true
null _ = false

snoc :: forall a. List a -> a -> List a
snoc Nil x = singleton x
snoc (y : ys) x = y : snoc ys x

length :: forall a. List a -> Int
length Nil = 0
length (_ : xs) = 1 + length xs

length' :: forall a. List a -> Int
length' l = go 0 l
  where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc (_ : xs) = go (acc + 1) xs

head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x : _) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing
tail (_ : xs) = Just xs

last :: forall a. List a -> Maybe a
last Nil = Nothing
last (x : Nil) = Just x
last (_ : xs) = last xs

init :: forall a. List a -> Maybe (List a)
init Nil = Nothing
init (_ : Nil) = Just Nil
init (x : xs) = Just $ x : fromMaybe Nil (init xs)

uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x : xs) = Just { head: x, tail: xs }

index :: forall a. List a -> Int -> Maybe a
index Nil _ = Nothing
index l i = go 0 l
  where
  go _ Nil = Nothing
  go ci (x : xs)
    | i < 0 = Nothing
    | ci == i = Just x
    | otherwise = go (ci + 1) xs

infixl 8 index as !!

findIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil = Nothing
findIndex p l = go 0 l
  where
  go :: Int -> List a -> Maybe Int
  go _ Nil = Nothing
  go ci (x : xs)
    | p x = Just ci
    | otherwise = go (ci + 1) xs

findLastIndex :: forall a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex p l = go 0 l Nothing
  where
  go :: Int -> List a -> Maybe Int -> Maybe Int
  go _ Nil acc = acc
  go ci (x : xs) acc
    | p x = go (ci + 1) xs (Just ci)
    | otherwise = go (ci + 1) xs acc

reverse :: List ~> List
reverse Nil = Nil
reverse (x : xs) = snoc (reverse xs) x

concat :: forall a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xs) = concat xs
concat ((x : xs) : xss) = x : concat (xs : xss)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter _ Nil = Nil
filter pred (x : xs)
  | pred x = x : filter pred xs
  | otherwise = filter pred xs

filter' :: forall a. (a -> Boolean) -> List a -> List a
filter' pred xs = reverse (go Nil xs)
  where
  go sl Nil = sl
  go sl (x : xs')
    | pred x = go (x : sl) xs'
    | otherwise = go sl xs'

catMaybes :: forall a. List (Maybe a) -> List a
catMaybes Nil = Nil
catMaybes (x : xs) = case x of
  Just y -> y : catMaybes xs
  Nothing -> catMaybes xs

range :: Int -> Int -> List Int
range start end = go Nil end start
  where
  go rl start' end'
    | start' == end' = start' : rl
    | otherwise = go (start' : rl) (start' + step) end'
  step = if start < end then (-1) else 1

take :: forall a. Int -> List a -> List a
take 0 _ = Nil
take _ Nil = Nil
take n (x : xs) = x : take (n - 1) xs

drop :: forall a. Int -> List a -> List a
drop 0 l = l
drop _ Nil = Nil
drop n (_ : xs) = drop (n - 1) xs

takeWhile :: forall a. (a -> Boolean) -> List a -> List a
takeWhile _ Nil = Nil
takeWhile pred (x : xs) =
  if pred x then x : takeWhile pred xs else Nil

dropWhile :: forall a. (a -> Boolean) -> List a -> List a
dropWhile _ Nil = Nil
dropWhile pred l@(x : xs) =
  if pred x then dropWhile pred xs else l

takeEnd :: forall a. Int -> List a -> List a
takeEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs
    # \tup@(Tuple c nl) -> if c < n then Tuple (c + 1) (x : nl) else tup

dropEnd :: forall a. Int -> List a -> List a
dropEnd n = go >>> snd
  where
  go Nil = Tuple 0 Nil
  go (x : xs) = go xs
    # \(Tuple c nl) -> Tuple (c + 1) $ if c < n then nl else x : nl

zip :: forall a b. List a -> List b -> List (Tuple a b)
zip _ Nil = Nil
zip Nil _ = Nil
zip (x : xs) (y : ys) = Tuple x y : zip xs ys

unzip :: forall a b. List (Tuple a b) -> Tuple (List a) (List b)
unzip Nil = Tuple Nil Nil
unzip (Tuple x y : ts) =
  unzip ts # \(Tuple xs ys) -> Tuple (x : xs) (y : ys)

test :: Effect Unit
test = do
  log $ show $ zip (1 : 2 : 3 : Nil) (4 : 5 : 6 : 7 : Nil)
  log $ show $ unzip ((Tuple 1 2) : (Tuple 3 4) : (Tuple 5 6) : Nil)
