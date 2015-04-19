-- | A data type and functions for working with ordered pairs and sequences of values.
module Data.Tuple where

import Control.Comonad (Comonad)
import Control.Extend (Extend)
import Control.Lazy (Lazy, defer)
import Data.Array (zipWith)
import Data.Monoid (Monoid, mempty)
import Data.Bifunctor (Bifunctor)
import Control.Biapply (Biapply)
import Control.Biapplicative (Biapplicative)

-- | A simple product type for wrapping a pair of component values.
data Tuple a b = Tuple a b

-- | Allows `Tuple`s to be rendered as a string with `show` whenever there are
-- | `Show` instances for both component types.
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show (Tuple a b) = "Tuple (" ++ show a ++ ") (" ++ show b ++ ")"

-- | Allows `Tuple`s to be checked for equality with `==` and `/=` whenever
-- | there are `Eq` instances for both component types.
instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2
  (/=) t1 t2 = not (t1 == t2)

-- | Allows `Tuple`s to be compared with `compare`, `>`, `>=`, `<` and `<=`
-- | whenever there are `Ord` instances for both component types. To obtain
-- | the result, the `fst`s are `compare`d, and if they are `EQ`ual, the
-- | `snd`s are `compare`d.
instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b) where
  compare (Tuple a1 b1) (Tuple a2 b2) = case compare a1 a2 of
    EQ -> compare b1 b2
    other -> other

instance boundedTuple :: (Bounded a, Bounded b) => Bounded (Tuple a b) where
  top = Tuple top top
  bottom = Tuple bottom bottom

instance semigroupoidTuple :: Semigroupoid Tuple where
  (<<<) (Tuple _ c) (Tuple a _) = Tuple a c

-- | The `Semigroup` instance enables use of the associative operator `<>` on
-- | `Tuple`s whenever there are `Semigroup` instances for the component
-- | types. The `<>` operator is applied pairwise, so:
-- | ```purescript
-- | (Tuple a1 b1) <> (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)
-- | ```
instance semigroupTuple :: (Semigroup a, Semigroup b) => Semigroup (Tuple a b) where
  (<>) (Tuple a1 b1) (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)

instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b) where
  mempty = Tuple mempty mempty

-- | The `Functor` instance allows functions to transform the contents of a
-- | `Tuple` with the `<$>` operator, applying the function to the second
-- | component, so:
-- | ```purescript
-- | f <$> (Tuple x y) = Tuple x (f y)
-- | ````
instance functorTuple :: Functor (Tuple a) where
  (<$>) f (Tuple x y) = Tuple x (f y)

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

-- | The `Functor` instance allows functions to transform the contents of a
-- | `Tuple` with the `<*>` operator whenever there is a `Semigroup` instance
-- | for the `fst` component, so:
-- | ```purescript
-- | (Tuple a1 f) <*> (Tuple a2 x) == Tuple (a1 <> a2) (f x)
-- | ```
instance applyTuple :: (Semigroup a) => Apply (Tuple a) where
  (<*>) (Tuple a1 f) (Tuple a2 x) = Tuple (a1 <> a2) (f x)

instance biapplyTuple :: Biapply Tuple where
  (<<*>>) (Tuple f g) (Tuple a b) = Tuple (f a) (g b)

instance applicativeTuple :: (Monoid a) => Applicative (Tuple a) where
  pure = Tuple mempty

instance biapplicativeTuple :: Biapplicative Tuple where
  bipure = Tuple

instance bindTuple :: (Semigroup a) => Bind (Tuple a) where
  (>>=) (Tuple a1 b) f = case f b of
    Tuple a2 c -> Tuple (a1 <> a2) c

instance monadTuple :: (Monoid a) => Monad (Tuple a)

instance extendTuple :: Extend (Tuple a) where
  (<<=) f t@(Tuple a b) = Tuple a (f t)

instance comonadTuple :: Comonad (Tuple a) where
  extract = snd

instance lazyTuple :: (Lazy a, Lazy b) => Lazy (Tuple a b) where
  defer f = Tuple (defer $ \_ -> fst (f unit)) (defer $ \_ -> snd (f unit))

-- | Returns the first component of a tuple.
fst :: forall a b. Tuple a b -> a
fst (Tuple a _) = a

-- | Returns the second component of a tuple.
snd :: forall a b. Tuple a b -> b
snd (Tuple _ b) = b

-- | Turn a function that expects a tuple into a function of two arguments.
curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
curry f a b = f (Tuple a b)

-- | Turn a function of two arguments into a function that expects a tuple.
uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
uncurry f (Tuple a b) = f a b

-- | Rakes two lists and returns a list of corresponding pairs.
-- | If one input list is short, excess elements of the longer list are discarded.
zip :: forall a b. [a] -> [b] -> [Tuple a b]
zip = zipWith Tuple

-- | Transforms a list of pairs into a list of first components and a list of
-- | second components.
unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
unzip ((Tuple a b):ts) = case unzip ts of
  Tuple as bs -> Tuple (a : as) (b : bs)
unzip [] = Tuple [] []

-- | Exchange the first and second components of a tuple.
swap :: forall a b. Tuple a b -> Tuple b a
swap (Tuple a b) = Tuple b a
