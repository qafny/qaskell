{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

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
import Data.Bits
import Data.List

import DSL.AdjMatrix
import DSL.Examples

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

-- TODO: Maybe we should use `StateT (Set w) ...` here instead of using
-- WriterT.
newtype Super w a = Super (WriterT (Set w) Fresh a)
  deriving (Functor, Applicative, Monad)

runSuper :: Super w a -> ([w], a)
runSuper (Super m) = swap $ fmap Set.toList $ runFresh $ runWriterT m
  where
    swap (a, b) = (b, a)

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

-- instance Ord a => FromList (Expr a) (Expr [a]) where
--   convertFromList = List

-- instance FromList w (Super w a w) where
--   convertFromList xs = Super xs _

-- generateChoicesFromList :: forall f t a b. (Applicative f, Traversable t) =>
--   [b] -> t a -> (t b -> r) -> r
-- generateChoicesFromList ds stuct f = undefined

class Monad m => GenChoices m b c where
  genChoices' :: (Traversable t) =>
    [b] -> t a -> m (t (a, c))

instance Ord b => GenChoices (Super b) b (Expr b) where
  genChoices' = genChoicesQuantum

instance GenChoices [] b b where
  genChoices' ds struct = 
    traverse (\a -> msum (map (go a) ds)) struct
    where
      go a d = return (a, d)

genChoicesQuantum :: forall a b t. (Ord b, Traversable t) =>
  [b] -> t a -> Super b (t (a, Expr b))
genChoicesQuantum choices struct = Super $ do
  tell $ Set.fromList choices
  lift $ traverse go struct
  where
    go :: a -> Fresh (a, Expr b)
    go x = do
      v <- fresh
      pure (x, Var v)

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

eqSumExample :: forall c m. (Num c, GenChoices m Int c) =>
  [Int] -> m c
eqSumExample inputList = do
  choice <- genChoices' @_ @_ @c [-1, 1 :: Int] inputList

  let multiplied = map (\(x, y) -> fromIntegral x * y) choice
  pure $ sum multiplied

runClassical :: forall b c. Super c (Expr c) -> [c]
runClassical act = do
  let (choices, expr) = runSuper act
      freeVars = getFreeVars expr

      sbsts = mkVarSubst freeVars choices

  sbst <- sbsts
  pure $ eval sbst expr

eqSumExampleInstance :: ([Int], Expr Int)
eqSumExampleInstance =
  runSuper (eqSumExample [7, 5, 10])

eqSumExampleInstanceClassical :: [Int]
eqSumExampleInstanceClassical = runClassical (eqSumExample [7, 5])

getBits :: (Num a, GetBit a) => Int -> a -> [a]
getBits bitSize x = go bitSize
  where
    go 0 = []
    go i =
      getBit x i : go (i-1)

neededBitSize :: Int -> Int
neededBitSize = ceiling . logBase 2 . fromIntegral

-- | Example:
-- ghci> runClassical (graphColoringExample 3 graph1) :: [Int]
-- ...
-- ghci> -- Syntactic version:
-- ghci> runSuper (graphColoringExample 3 graph1 :: Super Int (Expr Int))
-- ...
--
graphColoringExample :: forall a c m. (GetBit c, Num c, GenChoices m Int c) =>
  Int -> AdjMatrix a -> m c
graphColoringExample colorCount adj = do
  choice <- map snd <$> genChoices' @_ @_ @c [0..colorCount-1] nodes
  pure (go choice)
  where
    nodes :: [()]
    nodes = getNodes adj

    colorBitSize = neededBitSize colorCount

    edgeCalculation :: (c, c) -> c
    edgeCalculation (node1Color, node2Color) =
      let node1ColorBits = getBits colorBitSize node1Color
          node2ColorBits = getBits colorBitSize node2Color
      in
      sum (zipWith (*) node1ColorBits node2ColorBits)

    go :: [c] -> c
    go nodeColors =
      let adj' :: AdjMatrix (c, c)
          adj' = upperDiagonal $ updateNodeContents adj nodeColors
      in
      sum $ fmap edgeCalculation adj'

cliqueExample :: forall a c m. (Num c, GenChoices m Int c) =>
  Int -> AdjMatrix a -> m c
cliqueExample cliqueSize adj = do
    choice <- map snd <$> genChoices' @_ @_ @c [0, 1 :: Int] nodes
    pure (fromIntegral ((cliqueSize * (cliqueSize - 1)) `div` 2) * go choice)
  where
    nodes :: [()]
    nodes = getNodes adj

    edgeCalculation :: (c, c) -> c
    edgeCalculation (node1In, node2In) = node1In * node2In

    go :: [c] -> c
    go cliqueNodeChoices =
      let adj' :: AdjMatrix (c, c)
          adj' = upperDiagonal $ updateNodeContents adj cliqueNodeChoices
      in
      sum $ fmap edgeCalculation adj'

mkVarSubst :: [VarId] -> [a] -> [Subst a]
mkVarSubst freeVars choices =
  traverse (\var -> sequenceA (var, map Lit choices)) freeVars

data Expr a where
  -- Var :: [a] -> VarId -> Expr a
  Var :: VarId -> Expr a
  Lit :: a -> Expr a
  Add :: Expr Int -> Expr Int -> Expr Int
  Sub :: Expr Int -> Expr Int -> Expr Int
  Mul :: Expr Int -> Expr Int -> Expr Int

  GetBit :: Expr Int -> Int -> Expr Int

  -- SumList :: [Expr Integer] -> Expr Integer
  -- Intersect :: Expr [a] -> Expr [a] -> Expr [a]
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

eval :: Subst a -> Expr a -> a
eval sbst = go . substs sbst
  where
    go (Var v) = error $ "eval: Expression has free variable not contained in substitution/environment: " ++ show v
    go (Lit x) = x
    go (Add x y) = go x + go y
    go (Sub x y) = go x - go y
    go (Mul x y) = go x * go y
    go (GetBit x i) = getBit (go x) i

getFreeVars :: Expr a -> [VarId]
getFreeVars = Set.toList . go
  where
    go (Var v) = Set.singleton v
    go (Lit _) = mempty
    go (Add x y) = go x <> go y
    go (Sub x y) = go x <> go y
    go (Mul x y) = go x <> go y
    go (GetBit x _) = go x

subst :: VarId -> Expr a -> Expr a -> Expr a
subst var substExpr = go
  where
    go =
      \case
        Var var'
          | var' == var -> substExpr
          | otherwise   -> Var var'
        Lit x -> Lit x
        Add x y -> Add (go x) (go y)
        Sub x y -> Sub (go x) (go y)
        Mul x y -> Mul (go x) (go y)
        GetBit x i -> GetBit (go x) i

type Subst a = [(VarId, Expr a)]

substs :: Subst a -> Expr a -> Expr a
substs sbst e = foldr (\(var, substExpr) e' -> subst var substExpr e') e sbst

class GetBit a where
  getBit :: a -> Int -> a

instance GetBit Int where
  getBit x i =
    if testBit x i
    then 1
    else 0

instance GetBit (Expr Int) where
  getBit = GetBit

instance Num (Expr Int) where
  Lit 0 + y = y
  x + Lit 0 = x
  x + y = Add x y

  
  x * Lit 1 = x
  Lit 1 * y = y
  _ * Lit 0 = Lit 0
  Lit 0 * _ = Lit 0
  x * y = Mul x y

  x - Lit 0 = x
  x - y = Sub x y

  abs = error "Expr.abs"
  signum = error "Expr.signum"
  fromInteger = Lit . fromInteger

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

