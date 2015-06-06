-- | A data type and functions for working with ordered pairs.
module Data.Tuple where

import Prelude

import Control.Biapplicative (Biapplicative)
import Control.Biapply (Biapply)
import Control.Comonad (Comonad)
import Control.Extend (Extend)
import Control.Lazy (Lazy, defer)
import Data.Bifoldable (Bifoldable)
import Data.Bifunctor (Bifunctor)
import Data.Bitraversable (Bitraversable)
import Data.Foldable (Foldable, foldMap)
import Data.Functor.Invariant (Invariant, imapF)
import Data.Maybe (Maybe(..))
import Data.Maybe.First (First(..), runFirst)
import Data.Monoid (Monoid, mempty)
import Data.Traversable (Traversable)

-- | A simple product type for wrapping a pair of component values.
data Tuple a b = Tuple a b

-- | Allows `Tuple`s to be rendered as a string with `show` whenever there are
-- | `Show` instances for both component types.
instance showTuple :: (Show a, Show b) => Show (Tuple a b) where
  show (Tuple a b) = "Tuple (" ++ show a ++ ") (" ++ show b ++ ")"

-- | Allows `Tuple`s to be checked for equality with `==` and `/=` whenever
-- | there are `Eq` instances for both component types.
instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b) where
  eq (Tuple a1 b1) (Tuple a2 b2) = a1 == a2 && b1 == b2

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

instance boundedOrdTuple :: (BoundedOrd a, BoundedOrd b) => BoundedOrd (Tuple a b)

instance semigroupoidTuple :: Semigroupoid Tuple where
  compose (Tuple _ c) (Tuple a _) = Tuple a c

-- | The `Semigroup` instance enables use of the associative operator `<>` on
-- | `Tuple`s whenever there are `Semigroup` instances for the component
-- | types. The `<>` operator is applied pairwise, so:
-- | ```purescript
-- | (Tuple a1 b1) <> (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)
-- | ```
instance semigroupTuple :: (Semigroup a, Semigroup b) => Semigroup (Tuple a b) where
  append (Tuple a1 b1) (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)

instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b) where
  mempty = Tuple mempty mempty

instance semiringTuple :: (Semiring a, Semiring b) => Semiring (Tuple a b) where
  add (Tuple x1 y1) (Tuple x2 y2) = Tuple (add x1 x2) (add y1 y2)
  one = Tuple one one
  mul (Tuple x1 y1) (Tuple x2 y2) = Tuple (mul x1 x2) (mul y1 y2)
  zero = Tuple zero zero

instance moduloSemiringTuple :: (ModuloSemiring a, ModuloSemiring b) => ModuloSemiring (Tuple a b) where
  div (Tuple x1 y1) (Tuple x2 y2) = Tuple (div x1 x2) (div y1 y2)
  mod (Tuple x1 y1) (Tuple x2 y2) = Tuple (mod x1 x2) (mod y1 y2)

instance ringTuple :: (Ring a, Ring b) => Ring (Tuple a b) where
  sub (Tuple x1 y1) (Tuple x2 y2) = Tuple (sub x1 x2) (sub y1 y2)

instance divisionRingTuple :: (DivisionRing a, DivisionRing b) => DivisionRing (Tuple a b)

instance numTuple :: (Num a, Num b) => Num (Tuple a b)

instance booleanAlgebraTuple :: (BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (Tuple a b) where
  conj (Tuple x1 y1) (Tuple x2 y2) = Tuple (conj x1 x2) (conj y1 y2)
  disj (Tuple x1 y1) (Tuple x2 y2) = Tuple (disj x1 x2) (disj y1 y2)
  not (Tuple x y) = Tuple (not x) (not y)

-- | The `Functor` instance allows functions to transform the contents of a
-- | `Tuple` with the `<$>` operator, applying the function to the second
-- | component, so:
-- | ```purescript
-- | f <$> (Tuple x y) = Tuple x (f y)
-- | ````
instance functorTuple :: Functor (Tuple a) where
  map f (Tuple x y) = Tuple x (f y)

instance invariantTuple :: Invariant (Tuple a) where
  imap = imapF

instance bifunctorTuple :: Bifunctor Tuple where
  bimap f g (Tuple x y) = Tuple (f x) (g y)

-- | The `Functor` instance allows functions to transform the contents of a
-- | `Tuple` with the `<*>` operator whenever there is a `Semigroup` instance
-- | for the `fst` component, so:
-- | ```purescript
-- | (Tuple a1 f) <*> (Tuple a2 x) == Tuple (a1 <> a2) (f x)
-- | ```
instance applyTuple :: (Semigroup a) => Apply (Tuple a) where
  apply (Tuple a1 f) (Tuple a2 x) = Tuple (a1 <> a2) (f x)

instance biapplyTuple :: Biapply Tuple where
  biapply (Tuple f g) (Tuple a b) = Tuple (f a) (g b)

instance applicativeTuple :: (Monoid a) => Applicative (Tuple a) where
  pure = Tuple mempty

instance biapplicativeTuple :: Biapplicative Tuple where
  bipure = Tuple

instance bindTuple :: (Semigroup a) => Bind (Tuple a) where
  bind (Tuple a1 b) f = case f b of
    Tuple a2 c -> Tuple (a1 <> a2) c

instance monadTuple :: (Monoid a) => Monad (Tuple a)

instance extendTuple :: Extend (Tuple a) where
  extend f t@(Tuple a b) = Tuple a (f t)

instance comonadTuple :: Comonad (Tuple a) where
  extract = snd

instance lazyTuple :: (Lazy a, Lazy b) => Lazy (Tuple a b) where
  defer f = Tuple (defer $ \_ -> fst (f unit)) (defer $ \_ -> snd (f unit))

instance foldableTuple :: Foldable (Tuple a) where
  foldr f z (Tuple _ x) = f x z
  foldl f z (Tuple _ x) = f z x
  foldMap f (Tuple _ x) = f x

instance bifoldableTuple :: Bifoldable Tuple where
  bifoldMap f g (Tuple a b) = f a <> g b
  bifoldr f g z (Tuple a b) = f a (g b z)
  bifoldl f g z (Tuple a b) = g (f z a) b

instance traversableTuple :: Traversable (Tuple a) where
  traverse f (Tuple x y) = Tuple x <$> f y
  sequence (Tuple x y) = Tuple x <$> y

instance bitraversableTuple :: Bitraversable Tuple where
  bitraverse f g (Tuple a b) = Tuple <$> f a <*> g b
  bisequence (Tuple a b) = Tuple <$> a <*> b

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

-- | Exchange the first and second components of a tuple.
swap :: forall a b. Tuple a b -> Tuple b a
swap (Tuple a b) = Tuple b a

-- | Lookup a value in a data structure of `Tuple`s, generalizing association lists.
lookup :: forall a b f. (Foldable f, Eq a) => a -> f (Tuple a b) -> Maybe b
lookup a f = runFirst $ foldMap (\(Tuple a' b) -> First (if a == a' then Just b else Nothing)) f
