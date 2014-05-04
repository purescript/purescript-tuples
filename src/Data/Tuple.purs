module Data.Tuple where

import Data.Array
import Data.Monoid

data Tuple a b = Tuple a b
data Tuple3 a b c = Tuple3 a b c

instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show (Tuple a b) = "Tuple (" ++ show a ++ ") (" ++ show b ++ ")"

instance showTuple3 :: (Show a, Show b, Show c) => Show (Tuple3 a b c) where
  show (Tuple3 a b c) = "Tuple3(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")"

instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2
  (/=) t1 t2 = not (t1 == t2)

instance eqTuple3 :: (Eq a, Eq b, Eq c) => Eq (Tuple3 a b c) where
  (==) (Tuple3 a1 b1 c1) (Tuple3 a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2
  (/=) t1 t2 = not (t1 == t2)

instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b) where
  compare (Tuple a1 b1) (Tuple a2 b2) = case compare a1 a2 of
    EQ -> compare b1 b2
    other -> other

instance ordTuple3 :: (Ord a, Ord b, Ord c) => Ord (Tuple3 a b c) where
  compare (Tuple3 a1 b1 c1) (Tuple3 a2 b2 c2) = case compare a1 a2 of
    EQ -> case compare b1 b2 of
      EQ -> compare c1 c2
      other -> other
    other -> other

instance functorTuple :: Functor (Tuple a) where
  (<$>) f (Tuple x y) = Tuple x (f y)

instance applyTuple :: (Semigroup a) => Apply (Tuple a) where
  (<*>) (Tuple a1 f) (Tuple a2 x) = Tuple (a1 <> a2) (f x)

instance applicativeTuple :: (Monoid a) => Applicative (Tuple a) where
  pure = Tuple mempty

instance bindTuple :: (Semigroup a) => Bind (Tuple a) where
  (>>=) (Tuple a1 b) f = case f b of
    Tuple a2 c -> Tuple (a1 <> a2) c

instance monadTuple :: (Monoid a) => Monad (Tuple a)

fst :: forall a b. Tuple a b -> a
fst (Tuple a _) = a

snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b

curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
curry f a b = f (Tuple a b)

curry3 :: forall a b c d. (Tuple3 a b c -> d) -> a -> b -> c -> d
curry3 f a b c = f (Tuple3 a b c)

uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
uncurry f (Tuple a b) = f a b

uncurry3 :: forall a b c d. (a -> b -> c -> d) -> Tuple3 a b c -> d
uncurry3 f (Tuple3 a b c) = f a b c

zip :: forall a b. [a] -> [b] -> [Tuple a b]
zip = zipWith Tuple

unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
unzip ((Tuple a b):ts) = case unzip ts of
  Tuple as bs -> Tuple (a : as) (b : bs)
unzip [] = Tuple [] []

swap :: forall a b. Tuple a b -> Tuple b a
swap (Tuple a b) = Tuple b a
