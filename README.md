# Module Documentation

## Module Data.Tuple

#### `Tuple`

``` purescript
data Tuple a b
  = Tuple a b
```


#### `showTuple`

``` purescript
instance showTuple :: (Show a, Show b) => Show (Tuple a b)
```


#### `eqTuple`

``` purescript
instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)
```


#### `ordTuple`

``` purescript
instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b)
```


#### `semigroupoidTuple`

``` purescript
instance semigroupoidTuple :: Semigroupoid Tuple
```


#### `semigroupTuple`

``` purescript
instance semigroupTuple :: (Semigroup a, Semigroup b) => Semigroup (Tuple a b)
```


#### `monoidTuple`

``` purescript
instance monoidTuple :: (Monoid a, Monoid b) => Monoid (Tuple a b)
```


#### `functorTuple`

``` purescript
instance functorTuple :: Functor (Tuple a)
```


#### `applyTuple`

``` purescript
instance applyTuple :: (Semigroup a) => Apply (Tuple a)
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


#### `snd`

``` purescript
snd :: forall a b. Tuple a b -> b
```


#### `curry`

``` purescript
curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c
```


#### `uncurry`

``` purescript
uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c
```


#### `zip`

``` purescript
zip :: forall a b. [a] -> [b] -> [Tuple a b]
```


#### `zipWithIndex`

``` purescript
zipWithIndex :: forall a. [a] -> [Tuple a Number]
```


#### `unzip`

``` purescript
unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
```


#### `swap`

``` purescript
swap :: forall a b. Tuple a b -> Tuple b a
```



## Module Data.Tuple.Nested

#### `con2`

``` purescript
con2 :: forall a b z. (a -> b -> z) -> Tuple a b -> z
```


#### `con3`

``` purescript
con3 :: forall a b c z. (a -> b -> c -> z) -> Tuple a (Tuple b c) -> z
```


#### `con4`

``` purescript
con4 :: forall a b c d z. (a -> b -> c -> d -> z) -> Tuple a (Tuple b (Tuple c d)) -> z
```


#### `con5`

``` purescript
con5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> Tuple a (Tuple b (Tuple c (Tuple d e))) -> z
```


#### `con6`

``` purescript
con6 :: forall a b c d e f z. (a -> b -> c -> d -> e -> f -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f)))) -> z
```


#### `con7`

``` purescript
con7 :: forall a b c d e f g z. (a -> b -> c -> d -> e -> f -> g -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f g))))) -> z
```


#### `con8`

``` purescript
con8 :: forall a b c d e f g h z. (a -> b -> c -> d -> e -> f -> g -> h -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g h)))))) -> z
```


#### `con9`

``` purescript
con9 :: forall a b c d e f g h i z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h i))))))) -> z
```


#### `con10`

``` purescript
con10 :: forall a b c d e f g h i j z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z) -> Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h (Tuple i j)))))))) -> z
```


#### `(/\)`

``` purescript
(/\) :: forall a b. a -> b -> Tuple a b
```




