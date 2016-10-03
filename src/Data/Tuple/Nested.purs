-- | Utilities for n-tuples: sequences longer than two components built from
-- | nested pairs.
-- |
-- | Nested tuples arise naturally in product combinators. You shouldn't
-- | represent data using nested tuples, but if combinators you're working with
-- | create them, utilities in this module will allow to to more easily work
-- | with them, including translating to and from more traditional product types.
-- |
-- | ```purescript
-- | data Address = Address String City (Maybe Province) Country
-- |
-- | exampleAddress1 = makeAddress "221B Baker Street" London Nothing UK
-- | exampleAddress2 = makeAddressT $ "221B Baker Street" /\ London /\ Nothing /\ UK
-- |
-- | makeAddressT :: Tuple4 String City (Maybe Province) Country -> Address
-- | makeAddressT = uncurry4 Address
-- |
-- | makeAddress :: String -> City -> (Maybe Province) -> Country -> Address
-- | makeAddress = curry4 makeAddressT
-- |
-- | tupleAddress :: Address -> Tuple4 String City (Maybe Province) Country
-- | tupleAddress (Address a b c d) = tuple4 a b c d
-- | ```
module Data.Tuple.Nested where

import Data.Tuple (Tuple(..))

-- | Shorthand for constructing n-tuples as nested pairs.
-- | `a /\ b /\ c /\ d` becomes `Tuple (Tuple (Tuple a b) c ) d`
infixl 6 Tuple as /\

type Tuple2 = Tuple
type Tuple3 a b c = Tuple a (Tuple2 b c)
type Tuple4 a b c d = Tuple a (Tuple3 b c d)
type Tuple5 a b c d e = Tuple a (Tuple4 b c d e)
type Tuple6 a b c d e f = Tuple a (Tuple5 b c d e f)
type Tuple7 a b c d e f g = Tuple a (Tuple6 b c d e f g)
type Tuple8 a b c d e f g h = Tuple a (Tuple7 b c d e f g h)
type Tuple9 a b c d e f g h i = Tuple a (Tuple8 b c d e f g h i)
type Tuple10 a b c d e f g h i j = Tuple a (Tuple9 b c d e f g h i j)

-- | Given 2 values, creates a 2-tuple.
tuple2 :: forall a b. a -> b -> Tuple2 a b
tuple2 = Tuple

-- | Given 3 values, creates a nested 3-tuple.
tuple3 :: forall a b c. a -> b -> c -> Tuple3 a b c
tuple3 a b c = Tuple a (Tuple b c)

-- | Given 4 values, creates a nested 4-tuple.
tuple4 :: forall a b c d. a -> b -> c -> d -> Tuple4 a b c d
tuple4 a b c d = Tuple a (Tuple b (Tuple c d))

-- | Given 5 values, creates a nested 5-tuple.
tuple5 :: forall a b c d e. a -> b -> c -> d -> e -> Tuple5 a b c d e
tuple5 a b c d e = Tuple a (Tuple b (Tuple c (Tuple d e)))

-- | Given 6 values, creates a nested 6-tuple.
tuple6 :: forall a b c d e f. a -> b -> c -> d -> e -> f -> Tuple6 a b c d e f
tuple6 a b c d e f = Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f))))

-- | Given 7 values, creates a nested 7-tuple.
tuple7 :: forall a b c d e f g. a -> b -> c -> d -> e -> f -> g -> Tuple7 a b c d e f g
tuple7 a b c d e f g = Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f g)))))

-- | Given 8 values, creates a nested 8-tuple.
tuple8 :: forall a b c d e f g h. a -> b -> c -> d -> e -> f -> g -> h -> Tuple8 a b c d e f g h
tuple8 a b c d e f g h = Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g h))))))

-- | Given 9 values, creates a nested 9-tuple.
tuple9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> Tuple9 a b c d e f g h i
tuple9 a b c d e f g h i = Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h i)))))))

-- | Given 10 values, creates a nested 10-tuple.
tuple10 :: forall a b c d e f g h i j. a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Tuple10 a b c d e f g h i j
tuple10 a b c d e f g h i j = Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h (Tuple i j))))))))

-- | Given a function of 2 arguments, return a function that accepts a 2-tuple.
uncurry2 :: forall a b z. (a -> b -> z) -> Tuple2 a b -> z
uncurry2 f (Tuple t z) = f t z

-- | Given a function that accepts a 2-tuple, return a function of 2 arguments.
curry2 :: forall a b z. (Tuple2 a b -> z) -> a -> b -> z
curry2 f a b = f (Tuple a b)

-- | Given a function of 3 arguments, return a function that accepts a 3-tuple.
uncurry3 :: forall a b c z. (a -> b -> c -> z) -> Tuple3 a b c -> z
uncurry3 f (Tuple a (Tuple b c)) = f a b c

-- | Given a function that accepts a 3-tuple, return a function of 3 arguments.
curry3 :: forall a b c z. (Tuple3 a b c -> z) -> a -> b -> c -> z
curry3 f a b c = f (Tuple a (Tuple b c))

-- | Given a function of 4 arguments, return a function that accepts a 4-tuple.
uncurry4 :: forall a b c d z. (a -> b -> c -> d -> z) -> Tuple4 a b c d -> z
uncurry4 f (Tuple a (Tuple b (Tuple c d))) = f a b c d

-- | Given a function that accepts a 4-tuple, return a function of 4 arguments.
curry4 :: forall a b c d z. (Tuple4 a b c d -> z) -> a -> b -> c -> d -> z
curry4 f a b c d = f (Tuple a (Tuple b (Tuple c d)))

-- | Given a function of 5 arguments, return a function that accepts a 5-tuple.
uncurry5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> Tuple5 a b c d e -> z
uncurry5 f (Tuple a (Tuple b (Tuple c (Tuple d e)))) = f a b c d e

-- | Given a function that accepts a 5-tuple, return a function of 5 arguments.
curry5 :: forall a b c d e z. (Tuple5 a b c d e -> z) -> a -> b -> c -> d -> e -> z
curry5 f a b c d e = f (Tuple a (Tuple b (Tuple c (Tuple d e))))

-- | Given a function of 6 arguments, return a function that accepts a 6-tuple.
uncurry6 :: forall a b c d e f z. (a -> b -> c -> d -> e -> f -> z) -> Tuple6 a b c d e f -> z
uncurry6 f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f))))) = f' a b c d e f

-- | Given a function that accepts a 6-tuple, return a function of 6 arguments.
curry6 :: forall a b c d e f z. (Tuple6 a b c d e f -> z) -> a -> b -> c -> d -> e -> f -> z
curry6 f' a b c d e f = f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f)))))

-- | Given a function of 7 arguments, return a function that accepts a 7-tuple.
uncurry7 :: forall a b c d e f g z. (a -> b -> c -> d -> e -> f -> g -> z) -> Tuple7 a b c d e f g -> z
uncurry7 f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f g)))))) = f' a b c d e f g

-- | Given a function that accepts a 7-tuple, return a function of 7 arguments.
curry7 :: forall a b c d e f g z. (Tuple7 a b c d e f g -> z) -> a -> b -> c -> d -> e -> f -> g -> z
curry7 f' a b c d e f g = f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f g))))))

-- | Given a function of 8 arguments, return a function that accepts a 8-tuple.
uncurry8 :: forall a b c d e f g h z. (a -> b -> c -> d -> e -> f -> g -> h -> z) -> Tuple8 a b c d e f g h -> z
uncurry8 f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g h))))))) = f' a b c d e f g h

-- | Given a function that accepts a 8-tuple, return a function of 8 arguments.
curry8 :: forall a b c d e f g h z. (Tuple8 a b c d e f g h -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> z
curry8 f' a b c d e f g h = f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g h)))))))

-- | Given a function of 9 arguments, return a function that accepts a 9-tuple.
uncurry9 :: forall a b c d e f g h i z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> z) -> Tuple9 a b c d e f g h i -> z
uncurry9 f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h i)))))))) = f' a b c d e f g h i

-- | Given a function that accepts a 9-tuple, return a function of 9 arguments.
curry9 :: forall a b c d e f g h i z. (Tuple9 a b c d e f g h i -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> z
curry9 f' a b c d e f g h i = f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h i))))))))

-- | Given a function of 10 arguments, return a function that accepts a 10-tuple.
uncurry10 :: forall a b c d e f g h i j z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z) -> Tuple10 a b c d e f g h i j -> z
uncurry10 f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h (Tuple i j))))))))) = f' a b c d e f g h i j

-- | Given a function that accepts a 10-tuple, return a function of 10 arguments.
curry10 :: forall a b c d e f g h i j z. (Tuple10 a b c d e f g h i j -> z) -> a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z
curry10 f' a b c d e f g h i j = f' (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h (Tuple i j)))))))))
