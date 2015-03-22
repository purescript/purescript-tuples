-- | A data type and functions for working with ordered pairs and sequences of values.
module Data.Tuple where
  import Control.Comonad
  import Control.Extend
  import Control.Lazy

  import Data.Array
  import Data.Monoid

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

  instance semigroupoidTuple :: Semigroupoid Tuple where
    (<<<) (Tuple _ c) (Tuple a _) = Tuple a c

  -- | The `Semigroup` instance enables use of the associative operator `<>` on
  -- | `Tuple`s whenever there are `Semigroup` instances for the component types.
  -- | The `<>` operator is applied pairwise, so:
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

  -- | The `Functor` instance allows functions to transform the contents of a
  -- | `Tuple` with the `<*>` operator whenever there is a `Semigroup` instance
  -- | for the `fst` component, so:
  -- | ```purescript
  -- | (Tuple a1 f) <*> (Tuple a2 x) == Tuple (a1 <> a2) (f x)
  -- | ```
  instance applyTuple :: (Semigroup a) => Apply (Tuple a) where
    (<*>) (Tuple a1 f) (Tuple a2 x) = Tuple (a1 <> a2) (f x)

  instance applicativeTuple :: (Monoid a) => Applicative (Tuple a) where
    pure = Tuple mempty

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

  instance lazyLazy1Tuple :: (Lazy1 l1, Lazy1 l2) => Lazy (Tuple (l1 a) (l2 b)) where
    defer f = Tuple (defer1 $ \_ -> fst (f unit)) (defer1 $ \_ -> snd (f unit))

  instance lazyLazy2Tuple :: (Lazy2 l1, Lazy2 l2) => Lazy (Tuple (l1 a b) (l2 c d)) where
    defer f = Tuple (defer2 $ \_ -> fst (f unit)) (defer2 $ \_ -> snd (f unit))

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
  -- | If one input list is short, excess components of the longer list are discarded.
  zip :: forall a b. [a] -> [b] -> [Tuple a b]
  zip = zipWith Tuple

  -- | Transforms a list of pairs into a list of first components and a list of second components.
  unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]
  unzip ((Tuple a b):ts) = case unzip ts of
    Tuple as bs -> Tuple (a : as) (b : bs)
  unzip [] = Tuple [] []

  -- | Exchange the first and second components of a tuple.
  swap :: forall a b. Tuple a b -> Tuple b a
  swap (Tuple a b) = Tuple b a


-- | Utilities for n-tuples: sequences longer than two components built from nested pairs.
module Data.Tuple.Nested where
  import Data.Tuple

  -- | Given a function of 2 arguments, return a function that accepts a 2-tuple.
  con2 :: forall a b z. (a -> b -> z) -> (Tuple a b) -> z
  con2 f = \(Tuple a b) -> f a b

  -- | Given a function of 3 arguments, return a function that accepts a 3-tuple.
  con3 :: forall a b c z. (a -> b -> c -> z) -> (Tuple a (Tuple b c)) -> z
  con3 f = \(Tuple a (Tuple b c)) -> f a b c

  -- | Given a function of 4 arguments, return a function that accepts a 4-tuple.
  con4 :: forall a b c d z. (a -> b -> c -> d -> z) -> (Tuple a (Tuple b (Tuple c d))) -> z
  con4 f = \(Tuple a (Tuple b (Tuple c d))) -> f a b c d

  -- | Given a function of 5 arguments, return a function that accepts a 5-tuple.
  con5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> (Tuple a (Tuple b (Tuple c (Tuple d e)))) -> z
  con5 f = \(Tuple a (Tuple b (Tuple c (Tuple d e)))) -> f a b c d e

  -- | Given a function of 6 arguments, return a function that accepts a 6-tuple.
  con6 :: forall a b c d e f z. (a -> b -> c -> d -> e -> f -> z) -> (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f))))) -> z
  con6 f = \(Tuple a (Tuple b (Tuple c (Tuple d (Tuple e f'))))) -> f a b c d e f'

  -- | Given a function of 7 arguments, return a function that accepts a 7-tuple.
  con7 :: forall a b c d e f g z. (a -> b -> c -> d -> e -> f -> g -> z) -> (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f g)))))) -> z
  con7 f = \(Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f' g)))))) -> f a b c d e f' g

  -- | Given a function of 8 arguments, return a function that accepts a 8-tuple.
  con8 :: forall a b c d e f g h z. (a -> b -> c -> d -> e -> f -> g -> h -> z) -> (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g h))))))) -> z
  con8 f = \(Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f' (Tuple g h))))))) -> f a b c d e f' g h

  -- | Given a function of 9 arguments, return a function that accepts a 9-tuple.
  con9 :: forall a b c d e f g h i z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> z) -> (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h i)))))))) -> z
  con9 f = \(Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f' (Tuple g (Tuple h i)))))))) -> f a b c d e f' g h i

  -- | Given a function of 10 arguments, return a function that accepts a 10-tuple.
  con10 :: forall a b c d e f g h i j z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z) -> (Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f (Tuple g (Tuple h (Tuple i j))))))))) -> z
  con10 f = \(Tuple a (Tuple b (Tuple c (Tuple d (Tuple e (Tuple f' (Tuple g (Tuple h (Tuple i j))))))))) -> f a b c d e f' g h i j

  infixr 6 /\

  -- | Shorthand for constructing n-tuples as nested pairs.
  -- | `a /\ b /\ c /\ d` becomes `Tuple a (Tuple b (Tuple c d))`
  (/\) :: forall a b. a -> b -> Tuple a b
  (/\) a b = Tuple a b
