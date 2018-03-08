module Unify (unify, varInTerm) where

import Subst
import Type

-- | claculate the disagreement set of two terms
ds :: Term -> Term -> Maybe (Term, Term)
ds (Var i) (Var j) =
  if i == j
    then Nothing
    else Just (Var i, Var j)
ds (Var i) term = Just (Var i, term)
ds term (Var i) = Just (term, Var i)
ds (Comb str1 terms1) (Comb str2 terms2) =
  if str1 == str2 && length terms1 == length terms2
    then dsList terms1 terms2
    else Just (Comb str1 terms1, Comb str2 terms2)

dsList :: [Term] -> [Term] -> Maybe (Term, Term)
dsList [] [] = Nothing
dsList (t1:ts1) (t2:ts2) =
  case ds t1 t2 of
    Nothing -> dsList ts1 ts2
    x       -> x
dsList _ _ = error "Should not have happened!"


unify :: Term -> Term -> Maybe Subst
unify term1 term2 = unifyHelper term1 term2 empty

-- | Start with empty substitution and append everything else.
unifyHelper :: Term -> Term -> Subst -> Maybe Subst
unifyHelper term1 term2 subst =
  case ds (apply subst term1) (apply subst term2) of
    -- Disagreement set is empty
    Nothing -> Just subst
    --
    Just (Var i, Var j) ->
      unifyHelper term1 term2 (compose (single i (Var j)) subst) -- ^ create a single substitutuion
    Just (Comb _ _, Comb _ _) -> Nothing
    Just (Var i, term@(Comb _ terms)) ->
      if varInTerm i terms
        then Nothing -- TODO: disagreementSet takes this case
        else unifyHelper term1 term2 (compose (single i term) subst)
    Just (term@(Comb _ terms), Var i) ->
      if varInTerm i terms
        then Nothing
        else unifyHelper term1 term2 (compose (single i term) subst)

varInTerm :: VarIndex -> [Term] -> Bool
varInTerm _ [] = False
varInTerm i (Var index:xs) =
  if i == index
    then True
    else varInTerm i xs
varInTerm i (Comb _ term:xs) = varInTerm i term || varInTerm i xs
