module Main where

import System.Environment (getArgs)
import Quantum.Program
import Quantum.Examples
import Quantum.ExampleData

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

--   ("clique", "quantum")   -> print $ solveQuantum cliqueFinding
--   ("clique", "classical") -> print $ solveClassical cliqueFinding

--   ("cover", "quantum")    -> print $ solveQuantum exactCover
--   ("cover", "classical")  -> print $ solveClassical exactCover

--   ("infer", "quantum")    -> print $ solveQuantum inferType
--   ("infer", "classical")  -> print $ solveClassical inferType

  _ -> usage

usage :: IO ()
usage = do
  putStrLn "Usage:"
  putStrLn "  qaskell <example> <mode>"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  eqsum       # eqSum [1,2,3]"
  putStrLn "  color    # graphColoring 2 graph4"
--   putStrLn "  clique      # cliqueFinding"
--   putStrLn "  cover       # exactCover"
--   putStrLn "  infer       # inferType"
  putStrLn ""
  putStrLn "Modes:"
  putStrLn "  quantum     # Run quantum solver"
  putStrLn "  classical   # Run classical solver"
