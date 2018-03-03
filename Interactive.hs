{-# LANGUAGE ViewPatterns #-}
module Interactive where

import Data.List (intercalate, stripPrefix)
import Type
import Parser
import SLD
import Pretty
import Subst
import Unify

-- TODO: wissen nur durch load oder auch mit input?

main :: IO ()
main = do
  putStrLn "Welcome to Prolog!"
  putStrLn "Type \":help\" for help."
  loop dfs (Prog [])
  putStrLn "Halt!"

loop :: Strategy -> Prog -> IO ()
loop strat prog = do
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
    goal    ->  exec strat prog goal

-- parse and exec prolog code
exec :: Strategy -> Prog -> String -> IO ()
exec strat prog input =
  loop strat prog
--  case parseWithVars input of
--  Left err -> do
--    putStrLn ("Invalid input: " ++ err)
--    loop strat prog
--  Right (goal, list) -> do
--    putStrLn "Toll: "
--    loop strat prog
----      printResult output (solve strat prog goal)

-- print help message
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
-- TODO: sort alphabetically
info :: Prog -> IO ()
info (Prog rules) = do
  putStrLn "Available predicates:"
  putStrLn (intercalate "\n" (map infoHelper rules))
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
