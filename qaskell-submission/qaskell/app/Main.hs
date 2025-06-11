module Main where

import System.Environment (getArgs)
import Quantum.Program
import Quantum.Examples
import Quantum.ExampleData
import qualified Quantum.CtxInferType as CtxInfer

main :: IO ()
main = do
  args <- getArgs
  case args of
    [example, mode] -> runExample example mode
    _ -> usage

runExample :: String -> String -> IO ()
runExample example mode = case (example, mode) of
  ("eqsum", "quantum")    -> print $ solveQuantum (eqSum [1,3,2])
  ("eqsum", "classical")  -> print $ solveClassical (eqSum [1,3,2])

  ("color", "quantum")   -> print $ solveQuantum (graphColoring 2 graph4)
  ("color", "classical") -> print $ solveClassical (graphColoring 2 graph4)

  ("clique", "quantum")   -> print $ solveQuantum (cliqueFinding 3 graph1)
  ("clique", "classical") -> print $ solveClassical (cliqueFinding 3 graph1)

--   ("cover", "quantum")    -> print $ solveQuantum exactCover
--   ("cover", "classical")  -> print $ solveClassical exactCover

  ("infer1", "quantum")    -> print $ solveQuantum (inferType [("x", IntType)] expr1)
  ("infer1", "classical")  -> print $ solveClassical (inferType [("x", IntType)] expr1)
  
  ("infer", "quantum")    -> print $ solveQuantum (inferType [("x", IntType), ("y", IntType)] exprFull)
  ("infer", "classical")  -> print $ solveClassical (inferType [("x", IntType), ("y", IntType)] exprFull)

    -- The previous representation: --
  ("ctx-infer1", "quantum")    -> print $ solveQuantum (CtxInfer.inferType CtxInfer.expr1)
  ("ctx-infer1", "classical")  -> print $ solveClassical (CtxInfer.inferType CtxInfer.expr1)

  ("ctx-infer", "quantum")    -> print $ solveQuantum (CtxInfer.inferType CtxInfer.exprFull)
  ("ctx-infer", "classical")  -> print $ solveClassical (CtxInfer.inferType CtxInfer.exprFull)
  
  _ -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  qaskell <example> <mode>"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  eqsum       # eqSum [1,2,3]"
  putStrLn "  color    # graphColoring 2 graph4"
  putStrLn "  clique      # cliqueFinding"
  putStrLn "  infer       # inferType"
  putStrLn "  ctx-infer       # inferType"
  putStrLn ""
  putStrLn "Modes:"
  putStrLn "  quantum     # Run quantum solver"
  putStrLn "  classical   # Run classical solver"
