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

createBranches :: Prog -> Goal -> [(Subst, SLDTree)]
createBranches (Prog []) _ = []
createBranches
  prog@(Prog ((rule :- literals):restRules))
  goal@(Goal (firstGoal:restGoals)) =
    case unify rule firstGoal of
      Nothing  -> createBranches (Prog restRules) goal
      Just mgu -> [
                    (mgu, sld prog (Goal (map (apply mgu)
                    (literals ++ restGoals))))
                  ] ++ createBranches prog (Goal restGoals)
                    ++ createBranches (Prog restRules) goal

renameVars :: Prog -> Int -> Prog
renameVars (Prog rules) highest = Prog (renameRuleVars rules highest)
  where
    renameRuleVars :: [Rule] -> Int -> [Rule]
    renameRuleVars [] _ = []
    renameRuleVars ((term :- terms):restRules) highest =
      ((head (renameTermVars [term] highest)) :- (renameTermVars terms highest)) :
      (renameRuleVars restRules highest)
    renameTermVars :: [Term] -> Int -> [Term]
    renameTermVars [] _ = []
    renameTermVars ((Var i):restTerms) highest =
      (Var (i + highest)):(renameTermVars restTerms highest)
    renameTermVars ((Comb str terms):restTerms) highest =
      (Comb str (renameTermVars terms highest)) :
      (renameTermVars restTerms highest)

highestVar :: Goal -> Int -> Int
highestVar (Goal []) j = j
highestVar (Goal ((Comb str terms):restTerms)) j =
  highestVar (Goal restTerms) (highestVar (Goal terms) j)
highestVar (Goal ((Var i):restTerms)) j =
  if i > j
    then highestVar (Goal restTerms) i
    else highestVar (Goal restTerms) j

dfs :: Strategy
dfs (Node goal branches) = []

bfs :: Strategy
bfs (Node goal branches) = []

--solve :: Strategy -> Prog -> Goal -> [Subst]
