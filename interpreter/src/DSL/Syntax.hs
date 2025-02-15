{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}

module DSL.Syntax
  where

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad
import Control.Applicative
import Data.Proxy
import Data.Coerce
import Data.List (intersect, minimumBy)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Ord

-- data Super a = GenerateChoices [a] (a -> a)

-- data Super t a b energy where
--   GenerateChoices ::
--     t a -> -- | Input to original problem
--     [b] -> -- | Choices
--     -- (t (b, a) -> t c) -> -- | Setup initial state, which we use to produce a `m (t c)` internally
--     (t (b, a) -> energy) -> -- | What we map over the initial state
--     Super t a b energy

-- type SuperEval t m a b energy =
--   Super t a b energy -> m (t a)

-- -- | NOTE: Right now, this just finds the actual minimum. If we choose
-- -- `IO` for `m`, we could do random sampling.
-- classicalEval :: forall t m a b energy. (MonadPlus m, Foldable m, Traversable t, Ord energy) =>
--   SuperEval t m a b energy
-- classicalEval (GenerateChoices input choiceVals f) =
--   let choices = generateChoicesFromList @m choiceVals input
--   in
--   pure $ fmap snd $ minimumBy (comparing f) choices

type VarId = Int

newtype Fresh a = Fresh (State Int a)
  deriving (Functor, Applicative, Monad)

runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 0

fresh :: Fresh VarId
fresh = do
  curr <- Fresh get
  Fresh (modify (+1))
  pure curr

newtype Super w a = Super (WriterT (Set w) Fresh a)
  deriving (Functor, Applicative, Monad)

runSuper :: Super w a -> (a, [w])
runSuper (Super m) = fmap Set.toList $ runFresh $ runWriterT m

-- type Super' w = Super w w

-- -- TODO: Is this call to nub' enough for this to work right?
-- runSuper :: Ord w => Super w w b -> [b]
-- runSuper (Super xs f) = map f (nub' xs)

-- runSuperWithVars :: forall dummy a b. Super dummy (Expr a) b -> Fresh [b]
-- runSuperWithVars (Super xs f) = undefined
--   where
--     getFreshVars :: Fresh [Expr a]
--     getFreshVars =
--       traverse (\_ -> Var <$> fresh) xs

-- instance Functor (Super w a) where
--   fmap f (Super xs openTerm) = Super xs (f . openTerm)

-- instance Applicative (Super w a) where
--   pure x = Super mempty (\_ -> x)

--   Super xs f <*> Super ys g = Super (xs <> ys) (\v -> f v (g v))

class FromList a b | b -> a where
  convertFromList :: [a] -> b

instance FromList a [a] where
  convertFromList x = x

instance Ord a => FromList a (Super a (Expr b)) where

  convertFromList :: [a] -> Super a (Expr b)
  convertFromList xs = Super $ WriterT $ do
    v <- fresh
    pure (Var v, Set.fromList xs)

-- instance FromList w (Super w a w) where
--   convertFromList xs = Super xs _

-- generateChoicesFromList :: forall f t a b. (Applicative f, Traversable t) =>
--   [b] -> t a -> (t b -> r) -> r
-- generateChoicesFromList ds stuct f = undefined

generateChoicesFromList :: forall f t a b. (FromList b (f b), Applicative f, Traversable t) => 
                   [b] -> t a -> f (t b)
generateChoicesFromList ds struct =
  let r = fmap (\a -> convertFromList ds) struct
  in
  sequenceA r
--   -- traverse (\a -> _) struct
--   -- traverse (\a -> asum (map (go a) ds)) struct
  -- where
  --   go :: a -> b -> (b, a)
  --   go a d = (d, a)

data Expr a where
  Var :: [a] -> VarId -> Expr a
  Lit :: Int -> Expr Int
  Add :: Expr Int -> Expr Int -> Expr Int
  Sub :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int

  SumList :: Expr [Int] -> Expr Int
  Intersect :: Expr [a] -> Expr [a] -> Expr [a]
  -- Length :: Expr [a] -> Expr Int
  -- ListMap :: (Expr a -> Expr b) -> Expr [a] -> Expr [b]

  -- Gen :: Super a -> Expr (Super a)
  -- ChoiceMap :: (Expr a -> Expr b) -> Expr (Super a) -> Expr (Super b)
  -- Solve :: Super a -> Expr a

  -- Adjacency :: Structure Expr f => Expr (f a) -> Expr (f (a, a))

  -- ToList :: Structure Expr f => Expr (f a) -> Expr [a]

deriving instance Show a => Show (Expr a)
deriving instance Eq a => Eq (Expr a)
deriving instance Ord a => Ord (Expr a)

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
  -- adjacency :: f (g a) -> f (g (a, a)) -- TODO: Should this be more general
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

-- instance ListLike Expr where
--   getLength = Length
--   getIntersect = Intersect
--   sumList = SumList
--   listMap = ListMap

nub' :: Ord a => [a] -> [a]
nub' = Set.toList . Set.fromList

