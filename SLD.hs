module SLD (SLDTree (..), sld, highestVar, renameVars) where

import Type
import Subst
import Unify

data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving Show

-- | Creates an sld tree for a given program and request(goal)
sld :: Prog -> Goal -> SLDTree
sld (Prog []) goal = Node goal [] -- ^ Empty program gives empty tree
sld _ goal@(Goal []) = Node goal [] -- ^ Empty goal gives empty tree
sld prog goal = sldHelper (renameVars prog (highestVar goal 0 + 1)) goal
  where
    -- TODO: fix shadowing
    sldHelper :: Prog -> Goal -> SLDTree
    sldHelper prog1 goal1 = Node goal (createBranches prog1 prog1 goal1)

createBranches :: Prog -> Prog -> Goal -> [(Subst, SLDTree)]
createBranches _ (Prog []) _ = [] -- ^ End of recursion if there a no rules left
createBranches _ _ (Goal []) = []
-- ^ Create tree branch
-- ^ Get first rule of ruleset when ruleset is all rules of our
-- porgram we haven't applied yet.
createBranches fullProg (Prog ((rule :- literals):restRules))
  goal@(Goal (firstGoal:restGoals)) =
    -- ^ Try to unify a rule with a goal
    case unify rule firstGoal of
      -- ^ If rule could not be applied, try to applie rest rules to same goal.
      Nothing  -> createBranches fullProg (Prog restRules) goal
      Just mgu ->
        --TODO: hier fehler, dass goal im zweiten schritt leer !
        (mgu, sld fullProg (Goal (map (apply mgu) (literals ++ restGoals)))) :
        createBranches fullProg (Prog restRules) goal

renameVars :: Prog -> Int -> Prog
renameVars (Prog rules) highest = Prog (renameRuleVars rules highest)
  where
    renameRuleVars :: [Rule] -> Int -> [Rule]
    renameRuleVars [] _ = []
    renameRuleVars ((term :- terms):restRules) highest1 =
      (
        head (renameTermVars [term] highest1) :-
        (renameTermVars terms highest1)
      ) : renameRuleVars restRules highest1
    renameTermVars :: [Term] -> Int -> [Term]
    renameTermVars [] _ = []
    renameTermVars ((Var i):restTerms) highest1 =
      (Var (i + highest1)):(renameTermVars restTerms highest1)
    renameTermVars ((Comb str terms):restTerms) highest1 =
      (Comb str (renameTermVars terms highest1)) :
      (renameTermVars restTerms highest1)

highestVar :: Goal -> Int -> Int
highestVar (Goal []) j = j -- ^ for empty goal highest variable is input(ex.: 0)
highestVar (Goal ((Comb _ terms):restTerms)) j =
  -- No need to keep data structure, because we only want the highest var of all
  highestVar (Goal (terms ++ restTerms)) j
highestVar (Goal ((Var i):restTerms)) j = -- ^ check all request of a goal
  if i > j                                -- ^ Check if we found new max
    then highestVar (Goal restTerms) i    -- ^ Yes: call with new max
    else highestVar (Goal restTerms) j    -- ^ No: call with old max
