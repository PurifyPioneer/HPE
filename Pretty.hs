module Pretty (Pretty (..)) where

import Data.Char (chr)
import Data.List (intercalate, find)
import Type
import Subst

class Pretty a where
  pretty :: a -> String
  prettyWithVars :: [(VarIndex, String)] -> a -> String

instance Pretty Term where
  pretty term = prettyWithVars [] term

  prettyWithVars vars (Var i) =
    case find (\(index, str) -> index == i) vars of
      Just (_, str) -> str
      Nothing ->
        if i <= 25
          -- A to Z
          then chr (65 + i) : ""
          -- after Z comes A1, B1, ..., Z1, A2
          else chr (65 + (i `mod` 26)) : show (i `div` 26)

  prettyWithVars vars (Comb "." [x, (Comb "[]" [])]) = "[" ++ prettyWithVars vars x ++ "]"
  prettyWithVars vars (Comb "." [x, term@(Comb "." y)]) =
    "[" ++ prettyWithVars vars x ++ ", "
    -- deletes first and last list element, in our case parentheses
    ++ tail (init (prettyWithVars vars term)) ++ "]"
  prettyWithVars vars (Comb "." [x, y]) = "[" ++ prettyWithVars vars x ++ "|" ++ prettyWithVars vars y ++ "]"

  prettyWithVars vars (Comb str []) = str
  prettyWithVars vars (Comb str xs) = str ++ "(" ++ intercalate ", " (map (prettyWithVars vars) xs) ++ ")"

instance Pretty Subst where
  pretty subst = prettyWithVars [] subst

  prettyWithVars _ (Subst []) = ""
  prettyWithVars vars (Subst xs) = "{" ++ intercalate ", " (map (prettySubst vars) xs) ++ "}"
    where
      prettySubst vars (index, term) = prettyWithVars vars (Var index) ++ " -> " ++ prettyWithVars vars term
