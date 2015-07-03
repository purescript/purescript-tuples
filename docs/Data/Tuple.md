## Module Data.Tuple

A data type and functions for working with ordered pairs.

#### `Tuple`

``` purescript
data Tuple a b
  = Tuple a b
```

A simple product type for wrapping a pair of component values.

##### Instances
``` purescript
instance showTuple :: (Show a, Show b) => Show (Tuple a b)
instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)
instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b)
instance boundedTuple :: (Bounded a, Bounded b) => Bounded (Tuple a b)
instance boundedOrdTuple :: (BoundedOrd a, BoundedOrd b) => BoundedOrd (Tuple a b)
instance semigroupoidTuple :: Semigroupoid Tuple
instance semigroupTuple :: (Semigroup a, Semigroup b) => Semigroup (Tuple a b)
instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b)
instance semiringTuple :: (Semiring a, Semiring b) => Semiring (Tuple a b)
instance moduloSemiringTuple :: (ModuloSemiring a, ModuloSemiring b) => ModuloSemiring (Tuple a b)
instance ringTuple :: (Ring a, Ring b) => Ring (Tuple a b)
instance divisionRingTuple :: (DivisionRing a, DivisionRing b) => DivisionRing (Tuple a b)
instance numTuple :: (Num a, Num b) => Num (Tuple a b)
instance booleanAlgebraTuple :: (BooleanAlgebra a, BooleanAlgebra b) => BooleanAlgebra (Tuple a b)
instance functorTuple :: Functor (Tuple a)
instance invariantTuple :: Invariant (Tuple a)
instance bifunctorTuple :: Bifunctor Tuple
instance applyTuple :: (Semigroup a) => Apply (Tuple a)
instance biapplyTuple :: Biapply Tuple
instance applicativeTuple :: (Monoid a) => Applicative (Tuple a)
instance biapplicativeTuple :: Biapplicative Tuple
instance bindTuple :: (Semigroup a) => Bind (Tuple a)
instance monadTuple :: (Monoid a) => Monad (Tuple a)
instance extendTuple :: Extend (Tuple a)
instance comonadTuple :: Comonad (Tuple a)
instance lazyTuple :: (Lazy a, Lazy b) => Lazy (Tuple a b)
instance foldableTuple :: Foldable (Tuple a)
instance bifoldableTuple :: Bifoldable Tuple
instance traversableTuple :: Traversable (Tuple a)
instance bitraversableTuple :: Bitraversable Tuple
```

#### `fst`

``` purescript
fst :: forall a b. Tuple a b -> a
```

Returns the first component of a tuple.

#### `snd`

``` purescript
snd :: forall a b. Tuple a b -> b
```

Returns the second component of a tuple.

#### `curry`

``` purescript
curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
```

Turn a function that expects a tuple into a function of two arguments.

#### `uncurry`

``` purescript
uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
```

Turn a function of two arguments into a function that expects a tuple.

#### `swap`

``` purescript
swap :: forall a b. Tuple a b -> Tuple b a
```

Exchange the first and second components of a tuple.

#### `lookup`

``` purescript
lookup :: forall a b f. (Foldable f, Eq a) => a -> f (Tuple a b) -> Maybe b
```

Lookup a value in a data structure of `Tuple`s, generalizing association lists.


