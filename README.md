# Module Documentation

## Module Data.Tuple


A data type and functions for working with ordered pairs and sequences of values.

#### `Tuple`

``` purescript
data Tuple a b
  = Tuple a b
```

A simple product type for wrapping a pair of component values.

#### `showTuple`

``` purescript
instance showTuple :: (Show a, Show b) => Show (Tuple a b)
```

Allows `Tuple`s to be rendered as a string with `show` whenever there are
`Show` instances for both component types.

#### `eqTuple`

``` purescript
instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)
```

Allows `Tuple`s to be checked for equality with `==` and `/=` whenever
there are `Eq` instances for both component types.

#### `ordTuple`

``` purescript
instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b)
```

Allows `Tuple`s to be compared with `compare`, `>`, `>=`, `<` and `<=`
whenever there are `Ord` instances for both component types. To obtain
the result, the `fst`s are `compare`d, and if they are `EQ`ual, the
`snd`s are `compare`d.

#### `boundedTuple`

``` purescript
instance boundedTuple :: (Bounded a, Bounded b) => Bounded (Tuple a b)
```


#### `semigroupoidTuple`

``` purescript
instance semigroupoidTuple :: Semigroupoid Tuple
```


#### `semigroupTuple`

``` purescript
instance semigroupTuple :: (Semigroup a, Semigroup b) => Semigroup (Tuple a b)
```

The `Semigroup` instance enables use of the associative operator `<>` on
`Tuple`s whenever there are `Semigroup` instances for the component
types. The `<>` operator is applied pairwise, so:
```purescript
(Tuple a1 b1) <> (Tuple a2 b2) = Tuple (a1 <> a2) (b1 <> b2)
```

#### `monoidTuple`

``` purescript
instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b)
```


#### `functorTuple`

``` purescript
instance functorTuple :: Functor (Tuple a)
```

The `Functor` instance allows functions to transform the contents of a
`Tuple` with the `<$>` operator, applying the function to the second
component, so:
```purescript
f <$> (Tuple x y) = Tuple x (f y)
````

#### `applyTuple`

``` purescript
instance applyTuple :: (Semigroup a) => Apply (Tuple a)
```

The `Functor` instance allows functions to transform the contents of a
`Tuple` with the `<*>` operator whenever there is a `Semigroup` instance
for the `fst` component, so:
```purescript
(Tuple a1 f) <*> (Tuple a2 x) == Tuple (a1 <> a2) (f x)
```

#### `applicativeTuple`

``` purescript
instance applicativeTuple :: (Monoid a) => Applicative (Tuple a)
```


#### `bindTuple`

``` purescript
instance bindTuple :: (Semigroup a) => Bind (Tuple a)
```


#### `monadTuple`

``` purescript
instance monadTuple :: (Monoid a) => Monad (Tuple a)
```


#### `extendTuple`

``` purescript
instance extendTuple :: Extend (Tuple a)
```


#### `comonadTuple`

``` purescript
instance comonadTuple :: Comonad (Tuple a)
```


#### `lazyTuple`

``` purescript
instance lazyTuple :: (Lazy a, Lazy b) => Lazy (Tuple a b)
```


#### `lazyLazy1Tuple`

``` purescript
instance lazyLazy1Tuple :: (Lazy1 l1, Lazy1 l2) => Lazy (Tuple (l1 a) (l2 b))
```


#### `lazyLazy2Tuple`

``` purescript
instance lazyLazy2Tuple :: (Lazy2 l1, Lazy2 l2) => Lazy (Tuple (l1 a b) (l2 c d))
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

#### `zip`

``` purescript
zip :: forall a b. [a] -> [b] -> [Tuple a b]
```

Rakes two lists and returns a list of corresponding pairs.
If one input list is short, excess elements of the longer list are discarded.

#### `unzip`

``` purescript
unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
```

Transforms a list of pairs into a list of first components and a list of
second components.

#### `swap`

``` purescript
swap :: forall a b. Tuple a b -> Tuple b a
```

Exchange the first and second components of a tuple.