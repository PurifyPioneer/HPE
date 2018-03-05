module SLD (SLDTree (..), Strategy, sld, highestVar, renameVars) where

import Type
import Subst
import Unify

data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving Show

type Strategy = SLDTree -> [Subst]

-- | Creates an sld tree for a given program and request(goal)
sld :: Prog -> Goal -> SLDTree
sld (Prog []) goal = Node goal [] -- ^ Empty program gives empty tree
sld prog goal@(Goal []) = Node goal [] -- ^ Empty goal gives empty tree
sld prog goal = Node goal (createBranches (renameVars prog ((highestVar goal 0) + 1)) goal)

createBranches :: Prog -> Goal -> [(Subst, SLDTree)]
createBranches (Prog []) _ = [] -- ^ End of recursion if there a no rules left
createBranches  -- ^ Create tree branch
  -- ^ Get first rule of ruleset when ruleset is all rules of our
  -- porgram we haven't applied yet.
  prog@(Prog ((rule :- literals):restRules))
  -- TODO: comment
  goal@(Goal (firstGoal:restGoals)) =
    -- ^ Try to unify a rule with a goal
    case unify rule firstGoal of
      -- ^ If rule could not be applied, try to applie rest rules to same goal.
      Nothing  -> createBranches (Prog restRules) goal
      Just mgu -> (mgu, sld prog (Goal (map (apply mgu)
                    (literals ++ restGoals)))
                  )
                  : createBranches (Prog restRules) goal

renameVars :: Prog -> Int -> Prog
renameVars (Prog rules) highest = Prog (renameRuleVars rules highest)
  where
    renameRuleVars :: [Rule] -> Int -> [Rule]
    renameRuleVars [] _ = []
    renameRuleVars ((term :- terms):restRules) highest =
      (
        (head (renameTermVars [term] highest)) :-
        (renameTermVars terms highest)
      ) : (renameRuleVars restRules highest)
    renameTermVars :: [Term] -> Int -> [Term]
    renameTermVars [] _ = []
    renameTermVars ((Var i):restTerms) highest =
      (Var (i + highest)):(renameTermVars restTerms highest)
    renameTermVars ((Comb str terms):restTerms) highest =
      (Comb str (renameTermVars terms highest)) :
      (renameTermVars restTerms highest)

highestVar :: Goal -> Int -> Int
highestVar (Goal []) j = j -- ^ for empty goal highest variable is input(ex.: 0)
highestVar (Goal ((Comb _ terms):restTerms)) j =
  -- No need to keep data structure, because we only want the highest var of all
  highestVar (Goal (terms ++ restTerms)) j
highestVar (Goal ((Var i):restTerms)) j = -- ^ check all request of a goal
  if i > j                                -- ^ Check if we found new max
    then highestVar (Goal restTerms) i    -- ^ Yes: call with new max
    else highestVar (Goal restTerms) j    -- ^ No: call with old max
