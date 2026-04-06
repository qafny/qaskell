{-# LANGUAGE TupleSections #-}

module Quantum.Analysis where

import Quantum.Program
import Quantum.Examples (eqSum, sortP)

--import Data.Bits
import Data.Complex
import Data.Foldable (toList)
import Data.List (subsequences, sortOn, groupBy)
import Data.Function (on, (&))
import Control.Applicative

-- type C = Complex Double

todo = undefined

program = eqSum [2,1,3]
constr = constraints program
constr' = lift constr
v = 2
s = struct program
svid =  runFresh (genChoices s)
cs = choices program
ga = traverse  (\x -> (x , ) <$> cs) svid
ws = map (windows v) ga
costs = map (sum . map constr') ws


type Assignment t a b = t (Var a,b)
type Bits = [Int]
type Constr t a b c = t (a, b) -> c
type Constr' t a b c = Assignment t a b -> c
--   t (Var a,b) -> c

lift :: Traversable t => Constr t a b c -> Constr' t a b c
lift f = f .  fmap (\(Var x _, b) -> (x, b))

class Fillable f where
  fill :: f a -> [a] -> f a
  
instance Fillable [] where
  fill [] _          = []
  fill xs []         = xs
  fill (x:xs) (y:ys) = y : fill xs ys



-- from a traversble data points t a,
-- extracts windows -- all possible n-tuple of data pts a's
windows :: Traversable t => Int -> t a -> [[a]]
windows n t = filter ((== n) . length) $ subsequences $ toList t

-- all possible (full) assignments
genAssignments :: Traversable t => Program t a b c -> [Assignment t a b]
genAssignments (Program choices struct view constraints) =
    let struct_varid = runFresh (genChoices struct) -- struct marked w/e varid
     in traverse  (\x -> (x , ) <$> choices) struct_varid

-- number of variables/datapoints
varSize :: Traversable t => Program t a b c -> Int
varSize (Program _ struct _ _) =
  length $ toList struct

-- total qubits needed for this program
qbitSize :: Traversable t => Program t a b c -> Int
qbitSize p@(Program choices _ _ _) =
  (varSize p) * (neededBitSize $ length choices)


bitsToInt :: Bits -> Int
bitsToInt = foldl f 0
  where
    f acc b = acc*2 + b

bitsToStr :: Bits -> String
bitsToStr = map (\d -> if d == 0 then '0' else '1')

strToBits :: String -> Bits
strToBits = map (\d -> if d == '0' then 0 else 1)


-- converting big Endian bitcode back to choice
bitcodeToChoice :: Bits -> [b] -> Maybe b
bitcodeToChoice b choices =
  let u = length choices
      bid = bitsToInt b
   in if bid < u then Just $ choices !! bid else Nothing


choiceToBitcode :: Eq b => [b] -> b -> Maybe Bits
choiceToBitcode choices b =
  let
    u = length choices
    d = neededBitSize u
    idx = map (toBits d) [0..]
    ps = zip choices idx
   in lookup b ps

-- decode a bitstring/state back to a full assignment
-- some state are illegal (choice out of bound)
decodeState :: Traversable t => Program t a b c -> Bits -> Maybe (Assignment t a b)
decodeState p b =
  let v = varSize p
   in todo
      

encodeState :: (Traversable t, Eq b) => Program t a b c -> Assignment t a b -> Bits
encodeState prog@(Program choices struct view constraints) assgn =
  let
    ps = toList assgn
    ws = map f ps
    f (a,b) = case choiceToBitcode choices b of
      Just bits -> bits
      Nothing   -> error "encodeState: cannot find choice"
   in concat ws

{-
-- Table of States by Energy Level
-}

-- -- given a program, return each assignment's energy cost
--reference'  :: Traversable t => Program t a b c-> [(Assignment t a b, c)]
reference' prog@(Program choices struct view constraints) =
  let ga = genAssignments prog
      ws = map (windows view) ga
      constr = lift constraints
   in zip ga $ map (sum . map constr) ws

-- assignments sorted by energy
reference'' prog = sortOn snd (reference' prog)


-- given a program, returns each state's eigenvalue/energy cost
reference ::  (Eq b, Ord c, Num c) => Program [] a b c-> [(Bits, c)]
reference p =
  let r = reference'' p
      encode = encodeState p
   in map (\(assgn, c)-> (encode assgn, c)) r

optimalStates :: (Eq b, Ord c, Num c) => Program [] a b c-> [Bits]
optimalStates p =
  let
    rs = sortOn snd (reference p)
    rs' = groupBy ((==) `on` snd) rs
   in map fst $ head rs' 


ppPair (v, choice) = show v ++ " -> " ++ show choice
ppBlock [] = ""
ppBlock rs@((_,c):_) =
  let assgns  = map (show . map ppPair . fst) rs
   in show c ++ "\n" ++ unlines assgns
ppReference rs =
  let blocks = map ppBlock $ groupBy ((==) `on` snd) rs
   in mapM_ putStrLn blocks

example1 = ppReference $ reference'' (eqSum [2,1,3])
example2 = ppReference $ reference'' (sortP [2,1,3])



{-
-- Pauli String Eval
-}

evalPauli :: PauliExpr -> [Int] -> Int
evalPauli (I id) bits = 1
evalPauli (Z id) bits = if bits !! id == 0 then 1 else -1

evalTensorPauli :: Tensor PauliExpr -> [Int] -> Int
evalTensorPauli ps bits = foldr f 1 ps
  where
    f p acc = evalPauli p bits * acc

evalSSTP :: Summed (Scaled (Tensor PauliExpr)) -> [Int] -> Complex Double
evalSSTP stps bits = foldr f 0 stps
  where
    f (Scale c tp) acc = acc + c * (fromIntegral $ evalTensorPauli tp bits)



evalSSTP' :: Summed (Scaled (Tensor PauliExpr)) -> [Int] -> Double
evalSSTP' stps bits = evalSSTP stps bits & realPart

genBits :: Int -> [[Int]]
genBits 0 = []
genBits 1 = [[0],[1]]
genBits n = [ (x:xs) | x <- [0,1] , xs <- genBits (n-1)]

stps0 = solveQuantum $ eqSum [1,3,2]

states0 = zip (map bitsToStr $ genBits 3) $ map (evalSSTP' stps0) (genBits 3)

stps1 = solveQuantum $ sortP [2,1,3]
states1 = zip (genBits 6) $ map (evalSSTP' stps1) (genBits 6)
-- evalSSTP' stps1 [1,1,1,1,1,1] = 0 ???

-- extend the choices up to 2^d and punish the illegal choices 
preprocess :: (Eq b, Num b, Num c) => Program [] a b c -> Program [] a b c
preprocess prog@(Program choices struct view constraints) =
  let
    b   = length choices
    nbs = neededBitSize b
    b'  = 2 ^ nbs
    dif = b' - b
    
    magic_num = (- 99999)
    penalty = 10
    extended = choices ++ (take dif $ repeat magic_num)
    
    constr' pat@((a, choiceA) : _) =
        if magic_num `elem` map snd pat then penalty else constraints pat
   in
    Program extended struct view constr'

prp = preprocess (sortP [2,1,3])
stps2 = solveQuantum prp
states2 = zip (map bitsToStr $ genBits 6) $ map (evalSSTP' stps2) (genBits 6)
printst2 = putStrLn $ unlines $ map show states2

example3 = ppReference $ reference'' prp


stps3 = solveQuantum (sortP [2,1,3,4])
states3 =
  let q = 8
      s = stps3
   in zip (map bitsToStr $ genBits q) $ map (evalSSTP' s) (genBits q)
printst3 = putStrLn $ unlines $ map show states3
ref3 = ppReference $ reference'' (sortP [2,1,3,4])
