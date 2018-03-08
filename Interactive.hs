{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Interactive (main) where

import Data.List (intercalate, stripPrefix, nub, sort)
import System.IO

import Parser
import Pretty
import SLDSearch
import Subst
import Type

-- | Main entry point of program.
main :: IO ()
main = do
  putStrLn "Welcome to Prolog!"
  putStrLn "Type \":help\" for help."
  --loadFile dfs (Prog []) "test.pl"  -- ^ Used to load a file for easier debug.
  loop dfs (Prog [])                  -- ^ Start loop with empty program and
                                      -- ^ dfs as default strategy.
  putStrLn "Halt!"

-- | Loop  executing the program logic like getting input and executing goals.
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
      exec strat prog goal  -- ^ Executes a given goal.
      putStrLn "No"         -- ^ Indicate that there are no more soloutions.
      loop strat prog       -- ^ Continues with loop when goal is finished.

-- | Parses and executes a goal
exec :: Strategy -> Prog -> String -> IO ()
exec strat prog input =
  case (parseWithVars input :: Either String (Goal, [(VarIndex, String)])) of
  Left err -> do
    putStrLn err  -- ^ Print error if request could not be parsed.
  Right (goal, vars) -> do
    --putStrLn (show (sld prog goal)) -- ^ Debug: print out SLDTree
    printResult vars (solve strat prog goal) -- ^ Print the result of the goal.

-- | Prints a prolog result
printResult :: [(VarIndex, String)] -> [Subst] -> IO ()
printResult _ [] = return ()
printResult vars (subst:xs) = do
  putStrLn (prettyWithVars vars subst) -- ^ Print a single result.
  _ <- getLine                         -- ^ Used to print one result at a time.
  printResult vars xs                  -- ^ Print all further results.

-- | Prints an overview of all available functions the program offers.
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


-- | Print all available predicates.
info :: Prog -> IO ()
info (Prog rules) = do
  putStrLn "Available predicates:"
  -- intercalate with new lines and remove doubles
  putStrLn (intercalate "\n" (sort (nub (map infoHelper rules))))
  where
    -- | String representation of a rule including the number of arguments.
    infoHelper :: Rule -> String
    infoHelper (Comb str terms :- _) = str ++ "/" ++ show (length terms)
    infoHelper _ = ""

-- | Load program from file
loadFile :: Strategy -> Prog -> String -> IO ()
loadFile strat prog name =
  do
    file <- parseFile name
    case file of
      Left  err  -> do
        putStrLn ("Error in file loading: " ++ err)
        loop strat prog -- ^ Continue loop with old program
      Right newProg -> do
        putStrLn "File successfully loaded."
        loop strat newProg -- ^ Continue loop with newly loaded program

-- | Set search strategy
setStrategy :: Strategy -> Prog -> String -> IO ()
setStrategy strat prog input =
  case input of
    "bfs" -> do
      putStrLn "Now using breadth first search strategy."
      loop bfs prog -- ^ Continue loop with bfs.
    "dfs" -> do
      putStrLn "Now using depth first search strategy."
      loop dfs prog -- ^ Continue loop with dfs.
    _ -> do
      putStrLn "Invalid Strategy! Use bfs or dfs."
      loop strat prog -- ^ Continue loop with old strategy
