{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module DSL.Syntax
  where

import Control.Monad.Identity
import Control.Monad
import Data.Coerce
import Data.List (intersect, minimumBy)
import Data.Ord

-- data Super a = GenerateChoices [a] (a -> a)

data Super t a b energy where
  GenerateChoices ::
    t a -> -- | Input to original problem
    [b] -> -- | Choices
    -- (t (b, a) -> t c) -> -- | Setup initial state, which we use to produce a `m (t c)` internally
    (t (b, a) -> energy) -> -- | What we map over the initial state
    Super t a b energy

type SuperEval t m a b energy =
  Super t a b energy -> m (t a)

classicalEval :: forall t m a b energy. (MonadPlus m, Foldable m, Traversable t, Ord energy) =>
  SuperEval t m a b energy
classicalEval (GenerateChoices input choiceVals f) =
  let choices = generateChoicesFromList @m choiceVals input
  in
  pure $ fmap snd $ minimumBy (comparing f) choices

generateChoicesFromList :: (MonadPlus m, Traversable t) => 
                   [b] -> t a -> m (t (b, a))
generateChoicesFromList ds struct =
  traverse (\a -> msum (map (go a) ds)) struct
  where
    go a d = return (d, a)

data Expr a where
  Lit :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Sub :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int

  SumList :: Expr [Int] -> Expr Int
  Intersect :: Expr [a] -> Expr [a] -> Expr [a]
  Length :: Expr [a] -> Expr Int
  ListMap :: (Expr a -> Expr b) -> Expr [a] -> Expr [b]

  -- Gen :: Super a -> Expr (Super a)
  -- ChoiceMap :: (Expr a -> Expr b) -> Expr (Super a) -> Expr (Super b)
  -- Solve :: Super a -> Expr a

  Adjacency :: Structure Expr f => Expr (f a) -> Expr (f (a, a))

  ToList :: Structure Expr f => Expr (f a) -> Expr [a]

class Functor f => Zippable f g where
  -- zipWith' :: (a -> b -> c) -> f (g a) -> f (g b) -> f (g c)
  -- zipWithList :: (g a -> g b -> g c) -> f [a] -> f (g b) -> f (g c)
  fill :: f [a] -> f (g b) -> f (g a)
  fillPair :: f [a] -> f (g b) -> f (g (a, b))
  transposeFillPair :: f [a] -> f (g b) -> f (g (a, b))
  -- transposeFillWith :: (a -> b -> f c) -> f [a] -> f (g b) -> f (g c)

strength :: Functor g => (a, g b) -> g (a, b)
strength (x, gy) = fmap (\y -> (x, y)) gy

class Functor f => Structure f g where
  adjacency :: f (g a) -> f (g (a, a)) -- TODO: Should this be more general
  getList :: f (g a) -> f [a]
  getList_ :: f (g a) -> f [()]
  -- toQubits :: ...
  -- fromQubits :: ...

class ListLike f where
  getLength :: f [a] -> f Int
  getIntersect :: Eq a => f [a] -> f [a] -> f [a]
  sumList :: f [Int] -> f Int
  listMap :: (f a -> f b) -> f [a] -> f [b]

instance ListLike Identity where
  getLength = Identity . length . runIdentity

  getIntersect :: forall a. Eq a => Identity [a] -> Identity [a] -> Identity [a]
  getIntersect = coerce (intersect @a)

  sumList = Identity . sum . runIdentity

  listMap :: forall a b. (Identity a -> Identity b) -> Identity [a] -> Identity [b]
  listMap = coerce (map @a @b)

instance ListLike Expr where
  getLength = Length
  getIntersect = Intersect
  sumList = SumList
  listMap = ListMap

