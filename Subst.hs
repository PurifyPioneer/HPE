module Subst (Subst (..), empty, single, apply, compose) where

import Type

-- Data type for Substitutions
data Subst = Subst [(VarIndex, Term)]
  deriving Show

empty :: Subst
empty = Subst []

single :: VarIndex -> Term -> Subst
single i term = Subst [(i, term)]

apply :: Subst -> Term -> Term
apply (Subst []) term = term
apply (Subst ((index, termReplace):xs)) (Var i) =
  if index == i
    then termReplace
    else apply (Subst xs) (Var i)
apply subst (Comb str terms) = Comb str (map (apply subst) terms)

compose :: Subst -> Subst -> Subst
compose s1 (Subst []) = s1
compose (Subst []) s2 = s2
compose (Subst s1) (Subst s2) =
  Subst (
    (map (\(index, term) -> (index, apply (Subst s1) term)) s2) ++
    (filter (\ (index, term) -> notElem index (dom (Subst s2))) s1)
  )

dom :: Subst -> [VarIndex]
dom (Subst xs) = map fst xs
