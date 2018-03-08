module Pretty (Pretty (..)) where

import Data.Char (chr)
import Data.List (intercalate)

import Subst
import Type

-- | Typeclass for pretty printing
class Pretty a where
  -- ^ Pretty printing without considering variable names.
  pretty :: a -> String
  -- ^ Pretty printing that keeps original variable names.
  prettyWithVars :: [(VarIndex, String)] -> a -> String

-- | Instance of Pretty for printing terms
instance Pretty Term where
  pretty term = prettyWithVars [] term

  -- ^ Printing of term type Var.
  prettyWithVars vars (Var i) =
    case lookup i vars of
      Just str -> str
      Nothing  ->
        if i <= 25
          -- | A to Z
          then chr (65 + i) : ""
          -- | after Z comes A1, B1, ..., Z1, A2
          else chr (65 + (i `mod` 26)) : show (i `div` 26)

  -- ^ Printing of lists
  prettyWithVars vars (Comb "." [x, Comb "[]" []])      =
    "[" ++ prettyWithVars vars x ++ "]"
  prettyWithVars vars (Comb "." [x, term@(Comb "." _)]) =
    "[" ++ prettyWithVars vars x ++ ", " ++
    -- ^ Deletes opening "[" and closing "]" brackets.
    tail (init (prettyWithVars vars term)) ++ "]"
  prettyWithVars vars (Comb "." [x, y])                 =
    "[" ++ prettyWithVars vars x ++ "|" ++ prettyWithVars vars y ++ "]"

  -- ^ Printing of Comb terms and subterms.
  prettyWithVars _    (Comb str []) = str
  prettyWithVars vars (Comb str xs) =
    str ++ "(" ++ intercalate ", " (map (prettyWithVars vars) xs) ++ ")"

-- | Instance for pretty printing substitutions
instance Pretty Subst where
  pretty subst = prettyWithVars [] subst

  -- ^ interpret emty subst as successfull request (regarding an atom).
  -- ^ (a fail will be indicated by progam end with "no" Interactive:44)
  prettyWithVars _    (Subst []) = "true"
  prettyWithVars vars (Subst xs) =
    "{" ++ intercalate ", " (map (prettySubst vars) xs) ++ "}"
    where
      -- | Printing of a single substitution.
      prettySubst vars1 (index, term) =
        prettyWithVars vars1 (Var index) ++ " -> " ++ prettyWithVars vars1 term
