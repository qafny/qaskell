{-# LANGUAGE ScopedTypeVariables #-}

module Quantum.Program
  (Program (..)
  ,Var
  ,getVarPayload
  ,solveProgram
  )
  where

import Control.Monad.State
import Control.Monad

import Data.Foldable
import Data.List (nub)

import Numeric.LinearAlgebra hiding ((<>), toList)
import qualified Numeric.LinearAlgebra as Matrix
import Data.Complex
import Data.Bifunctor (second)

type VarId = Int

data PauliExpr = I VarId | Z VarId | Add PauliExpr PauliExpr | Sub PauliExpr PauliExpr | Scale (Complex Double) PauliExpr | Tensor PauliExpr PauliExpr

-- interpPauliExpr :: PauliExpr -> Pauli
-- interpPauliExpr I = ident 2
-- interpPauliExpr Z = pauliZ
-- interpPauliExpr (Add x y) = interpPauliExpr x + interpPauliExpr y
-- interpPauliExpr (Sub x y) = interpPauliExpr x - interpPauliExpr y
-- interpPauliExpr (Scale k x) = scalar k * (interpPauliExpr x)
-- interpPauliExpr (Tensor x y) = interpPauliExpr x Matrix.<> interpPauliExpr y

needsParens :: PauliExpr -> Bool
needsParens I{} = False
needsParens Z{} = False
needsParens (Add {}) = True
needsParens (Sub {}) = True
needsParens (Tensor {}) = True
needsParens (Scale {}) = False

parens :: String -> String
parens x = "(" ++ x ++ ")"

showParens :: PauliExpr -> String
showParens e =
  if needsParens e
  then parens (show e)
  else show e

instance Show PauliExpr where
  show (I i) = "I(" ++ [("xyz" ++ ['a'..'w']) !! i] ++ ")"
  show (Z i) = "Z(" ++ [("xyz" ++ ['a'..'w']) !! i] ++ ")"
  show (Add x y) = showParens x ++ " + " ++ showParens y
  show (Sub x y) = showParens x ++ " - " ++ showParens y
  show (Tensor x y) = showParens x ++ " ⊗ " ++ showParens y
  show (Scale k x) = prettyShow k ++ " " ++ showParens x
    where
      prettyShow (a :+ 0) = show a
      prettyShow (0 :+ b) = show b ++ "i"
      prettyShow (a :+ b) = parens (show a ++ " + " ++ show b ++ "i")

data Var a = Var a VarId
  deriving (Show, Eq, Ord)

getVarPayload :: Var a -> a
getVarPayload (Var x _) = x

getVarId :: Var a -> VarId
getVarId (Var _ i) = i

data Program t a =
  Program
    { choices :: [a]
    , struct :: t a
    , view :: Int
    , constraints :: [(Var a, a)] -> a
    }
generateVars :: Traversable t =>
  t a -> Fresh (t (Var a))
generateVars = traverse (\x -> Var x <$> fresh)

solveProgram :: forall t a. (Eq a, Traversable t) =>
  Program t a ->
  [(a, [PauliExpr])]
solveProgram prog =
  let
      varStruct :: t (Var a)
      varStruct = runFresh (generateVars (struct prog))

      pairs :: [[Var a]]
      pairs = distinctNTuples (view prog)
                              (toList varStruct)

      actualPairs :: [[(Var a, a)]]
      actualPairs = makeActualChoice (choices prog)
                                     pairs

      encodedChoices :: [(a, VarId -> PauliExpr)]
      encodedChoices = encodeChoices (choices prog)

      results :: [(a, [(Var a, a)])]
      results = map (\x -> (constraints prog x, x))
                    actualPairs

      decode :: (Var a, a) -> PauliExpr
      decode (var, choice) = decodeChoice encodedChoices choice (getVarId var)

      results2 :: [(a, [PauliExpr])]
      results2 = map (\(x, varChoices) -> (x, map decode varChoices)) results
  in
  results2

encodeChoices :: [a] -> [(a, VarId -> PauliExpr)]
encodeChoices choices = zipWith (\choice i -> (choice, toPauli choiceCount i)) choices [0..]
  where
    choiceCount = length choices

decodeChoice :: Eq a => [(a, s -> t)] -> a -> s -> t
decodeChoice encodedChoices choice var =
  case lookup choice encodedChoices of
    Just pauliFn -> pauliFn var

-- type Pauli = Matrix (Complex Double)

-- varsToPauli :: forall a. Eq a => [(a, [Var a])] -> [(a, [PauliExpr])]
-- varsToPauli xs =
--   let 
--       freeVars :: [Var a]
--       freeVars = nub $ concatMap snd xs -- Collect Vars and remove duplicates

--       totalVarCount = length freeVars
--   in
--   map (second (map (toPauli totalVarCount))) xs

toPauli :: Int -> Int -> (VarId -> PauliExpr)
toPauli totalChoiceCount i
  | i > totalChoiceCount = error "toPauli: i > totalChoiceCount"
  | i >= length allBitStrings = error "toPauli: i >= length allBitStrings"
  | otherwise = tensorBitString (allBitStrings !! i)
  where
    tensorBitString = foldr1 (liftA2 Tensor)

    pos x = Scale (1/2) (Sub (Z x) (I x))
    neg x = Scale (1/2) (Add (Z x) (I x))

    allBitStrings = replicateM bitSize [pos, neg]

    bitSize = neededBitSize totalChoiceCount

-- pauliZ :: Matrix (Complex Double)
-- pauliZ =
--   (2><2)
--   [ 1, 0
--   , 0, -1
--   ]

neededBitSize :: Int -> Int
neededBitSize = ceiling . logBase 2 . fromIntegral

makeActualChoice :: [b] -> [[a]] -> [[(a, b)]]
makeActualChoice choices xss = do
  xs <- xss
  ys <- replicateM (length xs) choices
  let zs = zip xs ys
  pure zs

-- | Postcondition: In the result of `distinctNTuples n xs`, the sublists
-- all have length `n`.
distinctNTuples :: Int -> [a] -> [[a]]
distinctNTuples n xs =
  filter ((== n) . length) $ filterM (const [False, True]) xs

newtype Fresh a = Fresh (State VarId a)
  deriving (Functor, Applicative, Monad)

runFresh :: Fresh a -> a
runFresh (Fresh m) = evalState m 0

fresh :: Fresh VarId
fresh = do
  x <- Fresh get
  Fresh $ modify (+1)
  pure x

