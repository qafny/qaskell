{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Eval
  where

import Lib

import Data.Complex
import Control.Monad

-- type Q = Double
-- type 

type Ix = Int

data Exp where
  Anni :: Ix -> Exp
  Const :: Basic -> Exp -> Exp
  Dagger :: Exp -> Exp
  Plus :: Exp -> Exp -> Exp
  Tensor :: Exp -> Exp -> Exp
  App :: Exp -> Exp -> Exp
  Zero :: Exp
  deriving (Show)

-- | Constraint language
data CExp where
  CAdd :: CExp -> CExp -> CExp
  CTimes :: CExp -> CExp -> CExp
  Exp :: Exp -> CExp
  deriving (Show)

cEval :: MonadPlus m => Env m -> CExp -> Env m
cEval env = \case
  CAdd e e' -> do
    v1 <- cEval env e
    v2 <- cEval env e'
    pure (tensorZipWith (+) v1 v2)

  CTimes e e' -> do
    v1 <- cEval env e
    v2 <- cEval env e'
    pure (tensorZipWith (*) v1 v2)

  Exp e -> eval env e

eval :: MonadPlus m => Env m -> Exp -> Env m
eval = evalStep eval evalDagger

-- | `evalDagger env e` should mean the same thing as `eval env (Dagger e)`
evalDagger :: MonadPlus m => Env m -> Exp -> Env m
evalDagger env = \case
  Anni j -> do
    s <- env

    let v = tensorIndex s j
        v' = conjugate v

    pure (singletonTensor v')

  Const k e -> do
    let k' = conjugate k

    s <- evalDagger env e
    pure (fmap (k' *) s)

  e -> evalStep evalDagger eval env e

-- | Run evaluation using the given function arguments for the 'recursive
-- call'
evalStep :: MonadPlus m => (Env m -> Exp -> Env m) -> (Env m -> Exp -> Env m) -> Env m -> Exp -> Env m
evalStep evalFn daggerFn env = \case
  Anni j -> do
    s <- env
    pure (singletonTensor (tensorIndex s j))

  Const k e -> do
    s <- evalFn env e
    pure (fmap (k *) s)

  Dagger e -> daggerFn env e

  Plus e e' ->
    evalFn env e `mplus` evalFn env e'

  Tensor e e' -> do
    eChoice <- evalFn env e
    e'Choice <- evalFn env e'

    pure (eChoice <> e'Choice)

  App e e' -> evalFn (evalFn env e') e

  Zero -> pure (singletonTensor 0) -- TODO: Is this right?

negateE :: Exp -> Exp
negateE = Const (-1)

summation :: Int -> Int -> (Int -> CExp) -> CExp
summation start end body
  | start >= end = Exp Zero
  | otherwise = CAdd (body start) (summation (start+1) end body)

pauliI :: Int -> Exp
pauliI j =
  Plus (App (Dagger (Anni j)) (Anni j))
       (App (Anni j) (Dagger (Anni j)))

-- TODO: Is this right?
pauliX :: Int -> Exp
pauliX j =
  Plus (Dagger (Anni j)) (Anni j)

pauliY :: Int -> Exp
pauliY j =
  let i = 0 :+ 1
  in
  Plus (Const i (Anni j))
       (Const (- i) (Dagger (Anni j)))

pauliZ :: Int -> Exp
pauliZ j =
  Plus (App (Anni j) (Dagger (Anni j)))
       (App (Dagger (negateE (Anni j))) (Anni j))


-- Examples --

equalSumZ' :: Int -> Exp
equalSumZ' j =
  Plus (App (Dagger (Anni j)) (Anni j))
       (App (Dagger (negateE (Anni j))) (Anni j))

equalSum :: Int -> (Int -> Basic) -> CExp
equalSum n g =
  CTimes
    -- TODO: Should this be 0 to n-1?
    (summation 0 n (\j -> Exp (Const (g j) (equalSumZ' j))))
    (summation 0 n (\j -> Exp (Const (g j) (equalSumZ' j))))




-- type Sigma = Int
--
-- type Type = (Int , Int)
--
-- data Op = Anni | Crea
--   deriving (Show)
--
-- data Exp = Anni
--         | Const Int Exp
--         | Ind Exp Nat
--         | Dagger Exp
--         | Plus Exp Exp
--         | Tensor Exp Exp
--         | App Exp Exp
--         deriving (Show)
--         
-- data Con = Val Int | Var String | Add Con Con | Dot Con Con
--          | Let String Exp Con
--          deriving (Show)
--
-- type State = [String -> Exp]
--
-- data V = A | B Int deriving (Show)
--
-- data S = List V
--
-- -- type val = List S
--
-- evalSingle :: V -> Exp -> List V
-- evalSingle A Anni = []
-- evalSingle (B v) Anni = [A]
-- evalSingle A (Dagger Anni) = [B 1]
-- evalSingle (B v) (Dagger Anni) = []
-- evalSingle v (Const z e) = case 
-- evalSingle A Const z e = A
--
-- timesE :: V -> Int -> V
-- timesE A v = A
-- timesE (B v1) v = B (v1 * v)
--
-- eval :: Nat -> List V -> Exp -> List V
-- eval n vs (App e1 e2) = eval n (eval n vs e2) e1
-- eval n vs (Tensor e1 e2) =
--           let v1,v2 = partition n vs
--               eval n v1 e1 ++ eval n v2 e2
--                            
-- eval n vs (Const z e) = do v <- eval n vs e
--                       return (timesE v z)
-- eval n vs (Ind e n) = do v <- eval n vs e
--                       return vs!!n -- vs[n] is the index n of vs
-- eval n vs e = do v <- vs
--                  case e of Anni | Dagger Anni -> return $ evalSingle v e
--                                
-- evals :: Nat -> List S -> Exp -> List S
-- evals n vs (Plus e1 e2) = evals n vs e1 ++ evals n vs e2
-- evals n vs e = do v <- vs
--                return eval n v e
--
-- evalpc :: State => Nat -> List S -> Exp -> List Int
-- evalpc n vs (Val i) = i
-- evalpc n vs (Var s) = do e <- Env s
--                          v <- eval n vs e
--                          return v
-- evalpc n vs (Add p1 p2) = va <- evalpc n vs p1
--                           vb <- evalpc n vs p2
--                           return va + vb
-- evalpc n vs (Dot p1 p2) = va <- evalpc n vs p1
--                           vb <- evalpc n vs p2
--                           return va * vb
-- evalpc n vs (Let x e p) = do put Env x e
--                           evalpc n vs p
--
-- --anti_s :: State -> Type -> State
-- --anti_s Zero _ = Zero
-- --anti_s (Pair n1 n2) t = Pair (t - n1) (t - n2)
--
-- gen_tensor :: Int -> Exp
-- gen_tensor 0 = I
-- gen_tensor n =  Tensor (gen_tensor (n - 1)) I
--
-- gen_list 0 = []
-- gen_list n = 0:(gen_list (n-1))
--
-- setAt i v l = (take i l) ++ (v:(drop (i+1) l))
--
-- gen_num :: Op -> Int -> State -> Exp
-- gen_num Anni s l = Hold l
-- gen_num Crea s l = Hold (setAt s 1 l)
--
-- equiv :: Exp -> Exp
-- equiv (App I I) = I
-- equiv (App (Plus e1 e2) e) = Plus (App e1 e) (App e2 e)
-- equiv (App (Tensor e1 e2) (Tensor e3 e4)) = Tensor (App e1 e3) (App e2 e4)
-- equiv (App (Single op s t) I) = Tensor (gen_tensor s) (Tensor (gen_num op s (gen_list (fst t))) (gen_tensor (fst t - s - 1))) 
--
-- interInt [1,0] = -1
-- interInt [0,1] = 1
-- interInt [0] = 0
-- interInt [1] = 1
--
-- interAux (Hold l) = [interInt l]
-- interAux (Tensor e1 e2) = interAux e1 ++ interAux e2
--
-- inter :: Exp -> [[Int]]
-- inter (Hold l) = [l]
-- inter (Tensor e1 e2) = [interAux (Tensor e1 e2)]
-- inter (Plus e1 e2) = inter e1 ++ inter e2
--
-- subst (Var x) y e = if x == y then Val e else (Var x)
-- subst (Val v) y e = Val v
-- subst (Let x e v) y ea = if x == y then Let x e v else Let x e (subst v y ea)
-- subst (Sum e1 e2) y ea = Sum (subst e1 y ea) (subst e2 y ea)
-- subst (Times e1 e2) y ea = Times e1 (subst e2 y ea)
--
-- listAddAux [] [] = []
-- listAddAux (v1:vl) (x1:xl) = (v1+x1):(listAddAux vl xl)
--
-- listAddA :: [Int] -> [[Int]] -> [[Int]]
-- listAddA v [] = []
-- listAddA v (x1:xl) = (listAddAux v x1) : listAddA v xl
--
-- listAdd :: [[Int]] -> [[Int]] -> [[Int]]
-- listAdd [] vl = []
-- listAdd (v1:vl) xl = listAddA v1 xl ++ listAdd vl xl
--
--
-- listTimesAux v [] = []
-- listTimesAux v (x:xl) = v * x : listTimesAux v xl
--
-- listTimes :: Int -> [[Int]] -> [[Int]]
-- listTimes v [] = []
-- listTimes v (x:xl) = (listTimesAux v x):(listTimes v xl)
--
--
-- dealC (Let x v c) = dealC (subst c x (inter (equiv v)))
-- dealC (Val xl) = xl
-- dealC (Sum e1 e2) = let v1 = dealC e1 in let v2 = dealC e2 in listAdd v1 v2
-- dealC (Times a e) = let v = dealC e in listTimes a v
--
--
--
--
--

