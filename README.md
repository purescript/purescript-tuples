# Module Documentation

## Module Data.Tuple

### Types

    data Tuple a b where
      Tuple :: a -> b -> Tuple a b

    data Tuple3 a b c where
      Tuple3 :: a -> b -> c -> Tuple3 a b c


### Type Class Instances

    instance applicativeTuple :: (Monoid a) => Applicative (Tuple a)

    instance applyTuple :: (Semigroup a) => Apply (Tuple a)

    instance bindTuple :: (Semigroup a) => Bind (Tuple a)

    instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)

    instance eqTuple3 :: (Eq a, Eq b, Eq c) => Eq (Tuple3 a b c)

    instance functorTuple :: Functor (Tuple a)

    instance monadTuple :: (Monoid a) => Monad (Tuple a)

    instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b)

    instance ordTuple3 :: (Ord a, Ord b, Ord c) => Ord (Tuple3 a b c)

    instance showTuple :: (Show a, Show b) => Show (Tuple a b)

    instance showTuple3 :: (Show a, Show b, Show c) => Show (Tuple3 a b c)


### Values

    curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c

    curry3 :: forall a b c d. (Tuple3 a b c -> d) -> a -> b -> c -> d

    fst :: forall a b. Tuple a b -> a

    snd :: forall a b. Tuple a b -> b

    swap :: forall a b. Tuple a b -> Tuple b a

    uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c

    uncurry3 :: forall a b c d. (a -> b -> c -> d) -> Tuple3 a b c -> d

    unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]

    zip :: forall a b. [a] -> [b] -> [Tuple a b]