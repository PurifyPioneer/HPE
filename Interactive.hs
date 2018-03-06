{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interactive (main) where

import System.IO
import Data.List (intercalate, stripPrefix, nub, sort)
import Type
import Parser
import SLD
import SLDSearch
import Pretty
import Subst
import Unify
import Test

main :: IO ()
main = do
  putStrLn "Welcome to Prolog!"
  putStrLn "Type \":help\" for help."
  loadFile dfs (Prog []) "test.pl" --debug autload
  --loop dfs (Prog [])
  putStrLn "Halt!"

loop :: Strategy -> Prog -> IO ()
loop strat prog = do
  hSetBuffering stdin LineBuffering
  putStr "?- "
  input <- getLine
  case input of
    -- https://stackoverflow.com/questions/1602243/pattern-matching-string-prefixes-in-haskell
    (stripPrefix ":load " -> Just str) -> loadFile strat prog str
    (stripPrefix ":set "  -> Just str) -> setStrategy strat prog str
    ":help" -> do
      help
      loop strat prog
    ":info" -> do
      info prog
      loop strat prog
    ":quit" ->  return ()
    goal ->  do
      exec strat prog goal
      loop strat prog

-- parse and exec prolog code
exec :: Strategy -> Prog -> String -> IO ()
exec strat prog input =
  case (parseWithVars input :: Either String (Goal, [(VarIndex, String)])) of
  Left err -> do
    putStrLn err
  Right (goal, vars) -> do
    printResult vars (solve strat prog goal)

-- prints a prolog result
printResult :: [(VarIndex, String)] -> [Subst] -> IO ()
printResult vars [] = return ()
printResult vars (subst:xs) = do
  putStrLn (prettyWithVars vars subst)
  printResult vars xs

-- prints help message
help :: IO ()
help = do
  putStrLn "Commands available from the prompt:"
  putStrLn "  <goal>        Solves/proves the specified goal."
  putStrLn "  :help         Shows this help message."
  putStrLn "  :info         Shows all available predicates."
  putStrLn "  :load <file>  Loads the specified file."
  putStrLn "  :quit         Exits the interactive environment."
  putStrLn "  :set <strat>  Sets the specified search strategy"
  putStrLn "                where <strat> is one of 'dfs' or 'bfs'."


-- print all available predicates
info :: Prog -> IO ()
info (Prog rules) = do
  putStrLn "Available predicates:"
  -- intercalate with new lines and remove doubles
  putStrLn (intercalate "\n" (sort (nub (map infoHelper rules))))
  where
    infoHelper :: Rule -> String
    infoHelper (Comb str terms :- _) = str ++ "/" ++ show (length terms)
    infoHelper _ = ""

-- load program from file
loadFile :: Strategy -> Prog -> String -> IO ()
loadFile strat prog name =
  do
    file <- parseFile name
    case file of
      Left  err  -> do
        putStrLn ("Error in file loading: " ++ err)
        loop strat prog
      Right newProg -> do
        putStrLn "File successfully loaded."
        loop strat newProg

-- set seach strategy
setStrategy :: Strategy -> Prog -> String -> IO ()
setStrategy strat prog ":set dfs" = loop strat prog
setStrategy strat prog input =
  case input of
    "bfs" -> do
      putStrLn "Now using breadth first search strategy."
      loop bfs prog
    "dfs" -> do
      putStrLn "Now using depth first search strategy."
      loop dfs prog
    _ -> do
      putStrLn "Invalid Strategy! Use bfs or dfs."
      loop strat prog
