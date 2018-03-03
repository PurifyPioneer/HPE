module SLD where

import Type
import Subst
import Unify

data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving Show

type Strategy = SLDTree -> [Subst]

-- TODO one tree per goal ?
sld :: Prog -> Goal -> SLDTree
sld (Prog []) goal = Node goal []
sld prog goal@(Goal []) = Node goal []
sld prog goal = Node goal (createBranches prog goal)
  where
    createBranches :: Prog -> Goal -> [(Subst, SLDTree)]
    createBranches (Prog []) _ = []
    createBranches prog@(Prog ((rule :- literals):restRules)) goal@(Goal (firstGoal:restGoals)) =
      case unify rule firstGoal of
        Nothing  -> createBranches (Prog restRules) goal
        Just mgu -> [(mgu, sld prog (Goal (map (apply mgu) (literals ++ restGoals))))]
                    ++ createBranches prog (Goal restGoals)
                    ++ createBranches (Prog restRules) goal

--    renameVars :: Prog -> Int -> Prog
--    renameVars (Prog ((Var i) :- terms)) highest = (Prog ((Var i + highest) :- map ) )
--      where
--        renameTermVars :: [Term] -> Int -> [Term]
--        renameTermVars [(Var i)] highest = Var (i + highest)
--        renameTermVars (term:rest) highest =

    highestVar :: Goal -> Int -> Int
    highestVar (Goal []) j = j
    highestVar (Goal ((Comb str terms):restTerms)) j = highestVar (Goal restTerms) (highestVar (Goal terms) j)
    highestVar (Goal ((Var i):restTerms)) j =
      if i > j
        then highestVar (Goal restTerms) i
        else highestVar (Goal restTerms) j

dfs :: Strategy
dfs (Node goal branches) = []

bfs :: Strategy
bfs (Node goal branches) = []

--solve :: Strategy -> Prog -> Goal -> [Subst]
