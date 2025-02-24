{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}

module H.TIQC where

import Data.List (nub)
import Data.Maybe (fromMaybe)
import Text.Printf (printf)

--------------------------------------------------------------------------------
-- Types and AST
--------------------------------------------------------------------------------

-- Type Variables
data TypeVar = TVarVar Int deriving (Eq, Show)

-- Our types: TInt, a multi-argument function TFunc [arg types] result,
-- or a type variable.
data Type
    = TInt
    | TFunc [Type] Type
    | TVarType Int
    deriving (Eq, Show)

-- Expressions (AST) in a curried lambda calculus.
-- Every node is annotated with a TypeVar.
data Expr
    = Var TypeVar
    | App TypeVar Expr Expr      -- Application: f x, annotated with a type variable.
    | Lambda TypeVar TypeVar Expr  -- Lambda abstraction: λx. body.
    | Tuple TypeVar Expr Expr      -- Tuple: (e1, e2)
    deriving (Eq, Show)

-- A constraint EqType v t means the type variable v must equal type t.
data Constraint = EqType TypeVar Type deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Constraint Generation and Unification
--------------------------------------------------------------------------------

-- A helper function that maps our types to an integer 0,1,2,3.
-- We assume the following:
--   TInt                   -> 0
--   TFunc [TInt] TInt      -> 1
--   TFunc [TInt, TInt] TInt  -> 2
--   TFunc [TInt, TInt, TInt] TInt -> 3

typeToInt :: Type -> Int
typeToInt TInt = 0
typeToInt (TFunc args ret) =
  case (args, ret) of
    ([TInt], TInt) -> 1
    ([TInt, TInt], TInt) -> 2
    ([TInt, TInt, TInt], TInt) -> 3
    _ -> 0  -- default fallback; more elaborate cases could be added
typeToInt (TVarType i) = i  -- fallback

generateConstraints :: Expr -> [Constraint]
generateConstraints expr = gen expr []
  where
    gen :: Expr -> [Int] -> [Constraint]
    gen (Var tv@(TVarVar i)) env =
      if i `elem` env then [] else [EqType tv TInt]
    gen (App (TVarVar v) f x) env =
      let cs_f   = gen f env
          cs_x   = gen x env
          fAnn   = getAnn f
          xAnn   = getAnn x
          cs_app = [EqType (TVarVar fAnn)
                        (TFunc [TVarType xAnn] (TVarType v))]
      in cs_f ++ cs_x ++ cs_app
    gen (Lambda (TVarVar v) (TVarVar param) body) env =
      let cs_body   = gen body (param : env)
          bodyAnn   = getAnn body
          cs_lambda = [EqType (TVarVar v)
                        (TFunc [TVarType param] (TVarType bodyAnn))]
      in cs_lambda ++ cs_body
    gen (Tuple (TVarVar v) e1 e2) env =
      let cs_e1    = gen e1 env
          cs_e2    = gen e2 env
          cs_tuple = [EqType (TVarVar v)
                        (TFunc [TVarType (getAnn e1), TVarType (getAnn e2)] TInt)]
      in cs_e1 ++ cs_e2 ++ cs_tuple

    getAnn :: Expr -> Int
    getAnn (Var (TVarVar i))          = i
    getAnn (App (TVarVar i) _ _)      = i
    getAnn (Lambda (TVarVar i) _ _)   = i
    getAnn (Tuple (TVarVar i) _ _)    = i

type Subst = [(Int, Type)]

occurs :: Int -> Type -> Bool
occurs _ TInt = False
occurs i (TFunc args ret) = any (occurs i) args || occurs i ret
occurs i (TVarType j) = i == j

applySubst :: Subst -> Type -> Type
applySubst s TInt = TInt
applySubst s (TFunc args ret) = TFunc (map (applySubst s) args) (applySubst s ret)
applySubst s (TVarType i) =
  case lookup i s of
    Just t  -> applySubst s t
    Nothing -> TVarType i

substituteInEquations :: Int -> Type -> [(Type, Type)] -> [(Type, Type)]
substituteInEquations i t = map (\(t1, t2) -> (subst t1, subst t2))
  where
    subst ty = case ty of
      TInt         -> TInt
      TFunc args r -> TFunc (map subst args) (subst r)
      TVarType j   -> if j == i then t else TVarType j

unifyAll :: [(Type, Type)] -> Either String Subst
unifyAll [] = Right []
unifyAll ((s, t):eqs)
  | s == t = unifyAll eqs
  | otherwise = case (s, t) of
      (TVarType i, _) ->
        if occurs i t then Left ("Occurs check failure: " ++ show i ++ " in " ++ show t)
        else do
          let eqs' = substituteInEquations i t eqs
          s' <- unifyAll eqs'
          return ((i, t) : s')
      (_, TVarType i) -> unifyAll ((TVarType i, s) : eqs)
      (TInt, TInt) -> unifyAll eqs
      (TFunc args1 ret1, TFunc args2 ret2) ->
         if length args1 /= length args2 then Left "Function arity mismatch"
         else unifyAll (zip args1 args2 ++ [(ret1, ret2)] ++ eqs)
      _ -> Left ("Type mismatch between " ++ show s ++ " and " ++ show t)

solveConstraints :: [Constraint] -> Either String [(TypeVar, Type)]
solveConstraints cs =
  let equations = [ (TVarType i, t) | EqType (TVarVar i) t <- cs ]
  in do
      s <- unifyAll equations
      let result = [ (TVarVar i, applySubst s (TVarType i)) | (i, _) <- s ]
      return result

--------------------------------------------------------------------------------
-- Example 1: (λx:int. x) y
--------------------------------------------------------------------------------

exampleExpr1 :: Expr
exampleExpr1 =
  App (TVarVar 4)
      (Lambda (TVarVar 2) (TVarVar 1) (Var (TVarVar 1)))
      (Var (TVarVar 3))

--------------------------------------------------------------------------------
-- Example 2: (\f. (\x. f x)) (\y. 5)
--------------------------------------------------------------------------------

exampleExpr2 :: Expr
exampleExpr2 =
  App (TVarVar 9)  -- The final result has type variable 9
    (Lambda (TVarVar 3) (TVarVar 1)   -- \f. (\x. f x)
      (Lambda (TVarVar 4) (TVarVar 2) 
        (App (TVarVar 5) (Var (TVarVar 1)) (Var (TVarVar 2)))  -- f x, using parameter 2
      )
    )
    (Lambda (TVarVar 6) (TVarVar 7)  -- \y. 5
      (Var (TVarVar 8))   -- 5
    )


--------------------------------------------------------------------------------
-- Pauli Hamiltonian Generation (Gate Instruction Output)
--------------------------------------------------------------------------------

-- We define a simple representation for Pauli operators.
data Pauli = I | Z deriving (Eq, Show)

type Qubit = Int
-- A PauliTerm is a coefficient and a list of (qubit, Pauli) factors.
type PauliTerm = (Double, [(Qubit, Pauli)])
type Hamiltonian = [PauliTerm]

-- Given a target integer (0,1,2,3) for a type variable, generate the local penalty Hamiltonian term
-- H = λ (V - t)^2, where V = (3 - 2Z(q1) - Z(q0))/2.
generatePenaltyTerms :: Int -> Int -> Double -> [PauliTerm]
generatePenaltyTerms varIndex target lambda =
  let q1 = 2 * (varIndex - 1)       -- first qubit for this variable (shifted)
      q0 = 2 * (varIndex - 1) + 1   -- second qubit for this variable (shifted)
      -- Coefficients derived from expanding (V - t)^2.
      c0 = lambda * (3.5 - 3 * fromIntegral target + fromIntegral target^2)
      c1 = lambda * (-3 + 2 * fromIntegral target)
      c0' = lambda * (-1.5 + fromIntegral target)
      c12 = lambda * 1
      term0 = (c0, [])                           -- Identity term (can be ignored for gate synthesis)
      term1 = (c1, [(q1, Z)])                    -- Z on qubit q1
      term2 = (c0', [(q0, Z)])                   -- Z on qubit q0
      term12 = (c12, [(q1, Z), (q0, Z)])         -- Z on both q1 and q0
  in [term0, term1, term2, term12]

-- A simple pretty printer for Hamiltonian terms.
prettyPrintHamiltonian :: Hamiltonian -> String
prettyPrintHamiltonian terms = unlines (map prettyTerm terms)
  where
    prettyTerm (coeff, ops) =
      show coeff ++ " * " ++ (if null ops then "I" else concatMap prettyOp ops)
    prettyOp (q, op) = " " ++ opStr op ++ "(q" ++ show q ++ ")"
    opStr I = "I"
    opStr Z = "Z"

--------------------------------------------------------------------------------
-- Main: Run type inference on Example 1 and generate its Pauli Hamiltonian
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "=== Example 1: (λx:int. x) y ==="
  let cs1 = generateConstraints exampleExpr1
  putStrLn "Generated Constraints for Example 1:"
  print cs1
  case solveConstraints cs1 of
    Left err -> putStrLn ("Unification error: " ++ err)
    Right sol -> do
      putStrLn "Type Inference Solution for Example 1:"
      print sol
      -- Generate the Hamiltonian with penalty coefficient lambda = 1.0.
      let ham = concatMap (\(TVarVar i, ty) ->
            let target   = typeToInt ty
                terms    = generatePenaltyTerms i target 1.0
                -- Remove the identity term (where the ops list is empty)
                nonIdTerms = filter (\(_, ops) -> not (null ops)) terms
            in map (\(coeff, ops) -> (2 * coeff, ops)) nonIdTerms) sol
      putStrLn "\nGenerated Pauli Hamiltonian (as a list of terms):"
      putStrLn (prettyPrintHamiltonian ham)

  putStrLn "\n=== Example 2: ((λf. (λx. f x)) (λy. 5)) ==="
  let cs2 = generateConstraints exampleExpr2
  putStrLn "Generated Constraints for Example 2:"
  print cs2
  case solveConstraints cs2 of
    Left err -> putStrLn ("Unification error: " ++ err)
    Right sol -> do
      putStrLn "Type Inference Solution for Example 2:"
      print sol
      let ham = concatMap (\(TVarVar i, ty) ->
            let target   = typeToInt ty
                terms    = generatePenaltyTerms i target 1.0
                nonIdTerms = filter (\(_, ops) -> not (null ops)) terms
            in map (\(coeff, ops) -> (2 * coeff, ops)) nonIdTerms) sol
      putStrLn "\nGenerated Pauli Hamiltonian (as a list of terms):"
      putStrLn (prettyPrintHamiltonian ham)

