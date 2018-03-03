module Pretty where

import Data.Char (chr)
import Data.List (intercalate)
import Type
import Subst

class Pretty a where
  pretty :: a -> String

instance Pretty Term where
  pretty (Var i) =
    if i <= 25
    -- A to Z
    then chr (65 + i) : ""
    -- after Z comes A1, B1, ..., Z1, A2
    else chr (65 + (i `mod` 26)) : show (i `div` 26)

  pretty (Comb "." [x, (Comb "[]" [])]) = "[" ++ pretty x ++ "]"
  pretty (Comb "." [x, term@(Comb "." y)]) =
    "[" ++ pretty x ++ ", "
    -- deletes first and last list element, in our case parentheses
    ++ tail (init (pretty term)) ++ "]"
  pretty (Comb "." [x, y]) = "[" ++ pretty x ++ "|" ++ pretty y ++ "]"

  pretty (Comb str []) = str
  pretty (Comb str xs) = str ++ "(" ++ intercalate ", " (map pretty xs) ++ ")"

instance Pretty Subst where
  pretty (Subst []) = ""
  pretty (Subst xs) = "{" ++ intercalate ", " (map prettySubst xs) ++ "}"
    where
      prettySubst (index, term) = pretty (Var index) ++ " -> " ++ pretty term
