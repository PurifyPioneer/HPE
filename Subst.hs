module Subst (Subst (..), empty, single, apply, compose) where

import Type

-- Data type for Substitutions
data Subst = Subst [(VarIndex, Term)]
  deriving Show

-- | Creates an empty substitution.
empty :: Subst
empty = Subst []

-- | Creates a single substitution.
single :: VarIndex -> Term -> Subst
single i term = Subst [(i, term)]

-- | Will apply a substitution to a terms and return the resulting term.
apply :: Subst -> Term -> Term
apply (Subst []) term = term  -- ^ empty substitution will give the same term
-- ^  Will replace a Var with a replacement term if indexes are matching.
apply (Subst ((index, termReplace):xs)) (Var i) =
  if index == i
    then termReplace  -- ^ Just replace the term.
    else apply (Subst xs) (Var i)  -- ^ Run apply with tail of substitutions.
-- ^ Will loop over sub terms of a Comb and "apply" to everyone.
apply subst (Comb str terms) = Comb str (map (apply subst) terms)


-- | Compose two substitutions with eachother.
compose :: Subst -> Subst -> Subst
compose s1 (Subst []) = s1  -- ^ Only one substitution is filled.
compose (Subst []) s2 = s2  -- ^ Only one substitution is filled.
compose (Subst s1) (Subst s2) =  -- ^ Combine to substitutions by appling.
  Subst (
    -- ^ Apply substitutions of s1 on s2.
    map (\(index, term) -> (index, apply (Subst s1) term)) s2 ++
    -- ^ Add all left and unused substitutions.
    filter (\(index, _) -> notElem index (dom (Subst s2))) s1
  )

-- | Gives a VarIndexes from substitution tuples.
dom :: Subst -> [VarIndex]
dom (Subst xs) = map fst xs
