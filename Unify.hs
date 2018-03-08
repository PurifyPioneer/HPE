module Unify (unify, varInTerm) where

import Subst
import Type

-- | Calculate the disagreement set of two terms.
ds :: Term -> Term -> Maybe (Term, Term)
-- ^ Compare two Vars with their indexes.
ds (Var i) (Var j) =
  if i == j
    then Nothing
    else Just (Var i, Var j)
-- ^ A Var and a Comb are not the same.
ds (Var i) term = Just (Var i, term)
-- ^ A Var and a Comb are not the same.
ds term (Var i) = Just (term, Var i)
-- ^ Compare two Combs by checking their names
-- and the length of their subterms.
ds (Comb str1 terms1) (Comb str2 terms2) =
  if str1 == str2 && length terms1 == length terms2
    then dsList terms1 terms2  -- ^ Test the subterms aswell.
    else Just (Comb str1 terms1, Comb str2 terms2)

-- | Helper for ds to compare two term lists.
dsList :: [Term] -> [Term] -> Maybe (Term, Term)
dsList []       []       = Nothing  -- ^ Terms are empty.
dsList (t1:ts1) (t2:ts2) =
  case ds t1 t2 of
    Nothing -> dsList ts1 ts2
    x       -> x
-- ^ Pattern to suppress 'non exhaustive' warning (will never be entered),
-- because we always check the terms lengths in ds.
dsList _        _        = error "Should not have happened!"

-- | Unify takes two terms and gives Nothing or mgu.
unify :: Term -> Term -> Maybe Subst
unify term1 term2 = unifyHelper term1 term2 empty  -- ^ Runs helper with empty
                                                   -- substitution.

-- | Start with empty substitution and append everything else.
unifyHelper :: Term -> Term -> Subst -> Maybe Subst
unifyHelper term1 term2 subst =
  case ds (apply subst term1) (apply subst term2) of
    -- ^ Disagreement set is empty.
    Nothing                           -> Just subst
    -- ^ Create a single substitutuion from i to j.
    Just (Var i, Var j)               ->
      unifyHelper term1 term2 (compose (single i (Var j)) subst)
    -- ^ We can't unify two diffrent Combs.
    Just (Comb _ _, Comb _ _)         -> Nothing
    -- ^ Check if we can find Var in Combs subterms.
    Just (Var i, term@(Comb _ terms)) ->
      if varInTerm i terms
        then Nothing
        else unifyHelper term1 term2 (compose (single i term) subst)
    -- ^ Check if we can find Var in Combs subterms.
    Just (term@(Comb _ terms), Var i) ->
      if varInTerm i terms
        then Nothing
        else unifyHelper term1 term2 (compose (single i term) subst)

-- | Find out whether a Var with VarIndex exists in a list of terms.
varInTerm :: VarIndex -> [Term] -> Bool
varInTerm _ []               = False  -- ^ Term list is empty.
varInTerm i (Var index:xs)   =        -- ^ Head of term list is a Var.
  if i == index
    then True
    else varInTerm i xs  -- ^ Check if var is in rest of list.
-- ^ Head of term list is a Comb.
varInTerm i (Comb _ term:xs) = varInTerm i term || varInTerm i xs
