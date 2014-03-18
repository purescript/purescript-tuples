module Data.Tuple where

import Prelude
import Data.Array

data Tuple a b = Tuple a b

fst :: forall a b. Tuple a b -> a
fst (Tuple a _) = a

snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b

instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show (Tuple a b) = "Tuple(" ++ show a ++ ", " ++ show b ++ ")"

curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
curry f a b = f (Tuple a b)

uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
uncurry f (Tuple a b) = f a b

zip :: forall a b. [a] -> [b] -> [Tuple a b]
zip = zipWith Tuple

unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
unzip ((Tuple a b):ts) = case unzip ts of
  Tuple as bs -> Tuple (a : as) (b : bs)
unzip [] = Tuple [] []

instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2
  (/=) t1 t2 = not (t1 == t2)

instance functorTuple :: Functor (Tuple a) where
  (<$>) f (Tuple x y) = Tuple x (f y)
