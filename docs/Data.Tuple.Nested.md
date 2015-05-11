## Module Data.Tuple.Nested

Utilities for n-tuples: sequences longer than two components built from
nested pairs.

Nested tuples arise naturally in product combinators. You shouldn't
represent data using nested tuples, but if combinators you're working with
create them, utilities in this module will allow to to more easily work
with them, including translating to and from more traditional product types.

```purescript
data Address = Address String City (Maybe Province) Country

exampleAddress1 = makeAddress "221B Baker Street" London Nothing UK
exampleAddress2 = makeAddressT $ "221B Baker Street" /\ London /\ Nothing /\ UK

makeAddressT :: Tuple4 String City (Maybe Province) Country -> Address
makeAddressT = uncurry4 Address

makeAddress :: String -> City -> (Maybe Province) -> Country -> Address
makeAddress = curry4 makeAddressT

tupleAddress :: Address -> Tuple4 String City (Maybe Province) Country
tupleAddress (Address a b c d) = tuple4 a b c d
```

#### `Tuple2`

``` purescript
type Tuple2 a z = Tuple a z
```

#### `Tuple3`

``` purescript
type Tuple3 a b z = Tuple (Tuple2 a b) z
```

#### `Tuple4`

``` purescript
type Tuple4 a b c z = Tuple (Tuple3 a b c) z
```

#### `Tuple5`

``` purescript
type Tuple5 a b c d z = Tuple (Tuple4 a b c d) z
```

#### `Tuple6`

``` purescript
type Tuple6 a b c d e z = Tuple (Tuple5 a b c d e) z
```

#### `Tuple7`

``` purescript
type Tuple7 a b c d e f z = Tuple (Tuple6 a b c d e f) z
```

#### `Tuple8`

``` purescript
type Tuple8 a b c d e f g z = Tuple (Tuple7 a b c d e f g) z
```

#### `Tuple9`

``` purescript
type Tuple9 a b c d e f g h z = Tuple (Tuple8 a b c d e f g h) z
```

#### `Tuple10`

``` purescript
type Tuple10 a b c d e f g h i z = Tuple (Tuple9 a b c d e f g h i) z
```

#### `tuple2`

``` purescript
tuple2 :: forall a b. a -> b -> Tuple2 a b
```

Given 2 values, creates a nested 2-tuple.

#### `tuple3`

``` purescript
tuple3 :: forall a b c. a -> b -> c -> Tuple3 a b c
```

Given 3 values, creates a nested 3-tuple.

#### `tuple4`

``` purescript
tuple4 :: forall a b c d. a -> b -> c -> d -> Tuple4 a b c d
```

Given 4 values, creates a nested 4-tuple.

#### `tuple5`

``` purescript
tuple5 :: forall a b c d e. a -> b -> c -> d -> e -> Tuple5 a b c d e
```

Given 5 values, creates a nested 5-tuple.

#### `tuple6`

``` purescript
tuple6 :: forall a b c d e f. a -> b -> c -> d -> e -> f -> Tuple6 a b c d e f
```

Given 6 values, creates a nested 6-tuple.

#### `tuple7`

``` purescript
tuple7 :: forall a b c d e f g. a -> b -> c -> d -> e -> f -> g -> Tuple7 a b c d e f g
```

Given 7 values, creates a nested 7-tuple.

#### `tuple8`

``` purescript
tuple8 :: forall a b c d e f g h. a -> b -> c -> d -> e -> f -> g -> h -> Tuple8 a b c d e f g h
```

Given 8 values, creates a nested 8-tuple.

#### `tuple9`

``` purescript
tuple9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> Tuple9 a b c d e f g h i
```

Given 9 values, creates a nested 9-tuple.

#### `tuple10`

``` purescript
tuple10 :: forall a b c d e f g h i j. a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Tuple10 a b c d e f g h i j
```

Given 10 values, creates a nested 10-tuple.

#### `uncurry2`

``` purescript
uncurry2 :: forall a b z. (a -> b -> z) -> Tuple2 a b -> z
```

Given a function of 2 arguments, return a function that accepts a 2-tuple.

#### `curry2`

``` purescript
curry2 :: forall a b z. (Tuple2 a b -> z) -> a -> b -> z
```

Given a function that accepts a 2-tuple, return a function of 2 arguments.

#### `uncurry3`

``` purescript
uncurry3 :: forall a b c z. (a -> b -> c -> z) -> Tuple3 a b c -> z
```

Given a function of 3 arguments, return a function that accepts a 3-tuple.

#### `curry3`

``` purescript
curry3 :: forall a b c z. (Tuple3 a b c -> z) -> a -> b -> c -> z
```

Given a function that accepts a 3-tuple, return a function of 3 arguments.

#### `uncurry4`

``` purescript
uncurry4 :: forall a b c d z. (a -> b -> c -> d -> z) -> Tuple4 a b c d -> z
```

Given a function of 4 arguments, return a function that accepts a 4-tuple.

#### `curry4`

``` purescript
curry4 :: forall a b c d z. (Tuple4 a b c d -> z) -> a -> b -> c -> d -> z
```

Given a function that accepts a 4-tuple, return a function of 4 arguments.

#### `uncurry5`

``` purescript
uncurry5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> Tuple5 a b c d e -> z
```

Given a function of 5 arguments, return a function that accepts a 5-tuple.

#### `curry5`

``` purescript
curry5 :: forall a b c d e z. (Tuple5 a b c d e -> z) -> a -> b -> c -> d -> e -> z
```

Given a function that accepts a 5-tuple, return a function of 5 arguments.

#### `uncurry6`

``` purescript
uncurry6 :: forall a b c d e f z. (a -> b -> c -> d -> e -> f -> z) -> Tuple6 a b c d e f -> z
```

Given a function of 6 arguments, return a function that accepts a 6-tuple.

#### `curry6`

``` purescript
curry6 :: forall a b c d e f z. (Tuple6 a b c d e f -> z) -> a -> b -> c -> d -> e -> f -> z
```

Given a function that accepts a 6-tuple, return a function of 6 arguments.

#### `uncurry7`

``` purescript
uncurry7 :: forall a b c d e f g z. (a -> b -> c -> d -> e -> f -> g -> z) -> Tuple7 a b c d e f g -> z
```

Given a function of 7 arguments, return a function that accepts a 7-tuple.

#### `curry7`

``` purescript
curry7 :: forall a b c d e f g z. (Tuple7 a b c d e f g -> z) -> a -> b -> c -> d -> e -> f -> g -> z
```

Given a function that accepts a 7-tuple, return a function of 7 arguments.

#### `uncurry8`

``` purescript
uncurry8 :: forall a b c d e f g h z. (a -> b -> c -> d -> e -> f -> g -> h -> z) -> Tuple8 a b c d e f g h -> z
```

Given a function of 8 arguments, return a function that accepts a 8-tuple.

#### `curry8`

``` purescript
curry8 :: forall a b c d e f g h z. (Tuple8 a b c d e f g h -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> z
```

Given a function that accepts a 8-tuple, return a function of 8 arguments.

#### `uncurry9`

``` purescript
uncurry9 :: forall a b c d e f g h i z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> z) -> Tuple9 a b c d e f g h i -> z
```

Given a function of 9 arguments, return a function that accepts a 9-tuple.

#### `curry9`

``` purescript
curry9 :: forall a b c d e f g h i z. (Tuple9 a b c d e f g h i -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> z
```

Given a function that accepts a 9-tuple, return a function of 9 arguments.

#### `uncurry10`

``` purescript
uncurry10 :: forall a b c d e f g h i j z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z) -> Tuple10 a b c d e f g h i j -> z
```

Given a function of 10 arguments, return a function that accepts a 10-tuple.

#### `curry10`

``` purescript
curry10 :: forall a b c d e f g h i j z. (Tuple10 a b c d e f g h i j -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z
```

Given a function that accepts a 10-tuple, return a function of 10 arguments.

#### `(/\)`

``` purescript
(/\) :: forall a b. a -> b -> Tuple a b
```

Shorthand for constructing n-tuples as nested pairs.
`a /\ b /\ c /\ d` becomes `Tuple (Tuple (Tuple a b) c ) d`


