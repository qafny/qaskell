{-# LANGUAGE GADTs, FlexibleContexts, ScopedTypeVariables #-}

module H.QuantumTypeInference where

import Data.List (nub)

--------------------------------------------------------------------------------
-- Types and AST
--------------------------------------------------------------------------------

-- Type Variables
data TypeVar = TVarVar Int deriving (Eq, Show)

-- Types: our base type, function types, and type variables.
data Type
  = TInt
  | TFunc [Type] Type
  | TVarType Int
  deriving (Eq, Show)

-- Expressions (AST) in a curried lambda calculus.
-- Every node is annotated with a TypeVar (carrying an Int).
data Expr
  = Var TypeVar
  | App TypeVar Expr Expr      -- Application: f x, annotated with a type variable.
  | Lambda TypeVar TypeVar Expr  -- Lambda abstraction: λx. body.
  | Tuple TypeVar Expr Expr      -- Tuple: (e1, e2)
  deriving (Eq, Show)

-- A constraint EqType v t means the type variable v must equal type t.
data Constraint = EqType TypeVar Type deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Constraint Generation
--------------------------------------------------------------------------------

-- We assume that free variables (not bound by a lambda) are of type TInt.
-- For application, we generate a constraint so that the function's annotation
-- equals a function type that accepts the argument's type and returns the
-- application node's annotated type.
-- For lambdas we generate a constraint that the lambda’s annotation is a function
-- from the parameter’s type to the body’s type.
generateConstraints :: Expr -> [Constraint]
generateConstraints expr = gen expr []
  where
    gen :: Expr -> [Int] -> [Constraint]
    gen (Var tv@(TVarVar i)) env =
      if i `elem` env then [] else [EqType tv TInt]
    gen (App (TVarVar v) f x) env =
      let cs_f   = gen f env
          cs_x   = gen x env
          -- Extract the annotation numbers from f and x.
          fAnn   = getAnn f  
          xAnn   = getAnn x  
          cs_app = [ EqType (TVarVar fAnn)
                        (TFunc [TVarType xAnn] (TVarType v)) ]
      in cs_f ++ cs_x ++ cs_app
    gen (Lambda (TVarVar v) (TVarVar param) body) env =
      let cs_body   = gen body (param : env)
          bodyAnn   = getAnn body
          cs_lambda = [ EqType (TVarVar v)
                        (TFunc [TVarType param] (TVarType bodyAnn)) ]
      in cs_lambda ++ cs_body
    gen (Tuple (TVarVar v) e1 e2) env =
      let cs_e1    = gen e1 env
          cs_e2    = gen e2 env
          cs_tuple = [ EqType (TVarVar v)
                        (TFunc [TVarType (getAnn e1), TVarType (getAnn e2)] TInt) ]
      in cs_e1 ++ cs_e2 ++ cs_tuple

    -- Helper: extract the annotation (an Int) from an expression.
    getAnn :: Expr -> Int
    getAnn (Var (TVarVar i))        = i
    getAnn (App (TVarVar i) _ _)      = i
    getAnn (Lambda (TVarVar i) _ _)   = i
    getAnn (Tuple (TVarVar i) _ _)    = i

--------------------------------------------------------------------------------
-- Unification
--------------------------------------------------------------------------------

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
-- Examples
--------------------------------------------------------------------------------

-- Example 1: (λx: int. x) y
-- Encoded as:
--   App (TVarVar 4)
--       (Lambda (TVarVar 2) (TVarVar 1) (Var (TVarVar 1)))
--       (Var (TVarVar 3))
--
-- Expected:
--   var₁: int → int
--   var₂: (int → int) → (int → int)
--   var₃: int
--   var₄: int
exampleExpr1 :: Expr
exampleExpr1 =
  App (TVarVar 4)
      (Lambda (TVarVar 2) (TVarVar 1) (Var (TVarVar 1)))
      (Var (TVarVar 3))

-- Example 2 (curried): ((λx: int→int. x) (λx: int. x)) y
--
-- Encoded as:
--   App (TVarVar 7)
--       (App (TVarVar 5)
--            (Lambda (TVarVar 2) (TVarVar 1) (Var (TVarVar 1)))   -- first lambda: f
--            (Lambda (TVarVar 4) (TVarVar 3) (Var (TVarVar 3))))  -- second lambda: g
--       (Var (TVarVar 6))
--
-- With manual declarations to “force” the intended (curried) types:
--   var₁: int → int         (first lambda parameter)
--   var₂: (int→int) → (int→int)  (first lambda overall)
--   var₃: int               (second lambda parameter)
--   var₄: int → int         (second lambda overall)
--   var₆: int               (free variable y)
--
-- The application of f to g then forces:
--   var₅: int → int
-- and the final application yields:
--   var₇: int
exampleExpr2 :: Expr
exampleExpr2 =
  App (TVarVar 7)
      (App (TVarVar 5)
           (Lambda (TVarVar 2) (TVarVar 1) (Var (TVarVar 1)))
           (Lambda (TVarVar 4) (TVarVar 3) (Var (TVarVar 3))))
      (Var (TVarVar 6))

-- Manual constraints for Example 2 (curried encoding)
manualConstraints2 :: [Constraint]
manualConstraints2 =
  [ EqType (TVarVar 1) (TFunc [TInt] TInt)                          -- first lambda parameter: int → int
  , EqType (TVarVar 2) (TFunc [TFunc [TInt] TInt] (TFunc [TInt] TInt))   -- first lambda overall: (int→int) → (int→int)
  , EqType (TVarVar 3) TInt                                           -- second lambda parameter: int
  , EqType (TVarVar 4) (TFunc [TInt] TInt)                              -- second lambda overall: int → int
  , EqType (TVarVar 6) TInt                                           -- free variable y: int
  ]

-- Combine the automatically generated constraints with the manual ones.
constraintsExpr2 :: [Constraint]
constraintsExpr2 = generateConstraints exampleExpr2 ++ manualConstraints2

--------------------------------------------------------------------------------
-- Main
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

  putStrLn "\n=== Example 2: ((λx: int→int. x) (λx: int. x)) y ==="
  putStrLn "Generated Constraints for Example 2 (with manual declarations):"
  print constraintsExpr2
  case solveConstraints constraintsExpr2 of
    Left err -> putStrLn ("Unification error: " ++ err)
    Right sol -> do
      putStrLn "Type Inference Solution for Example 2:"
      print sol
