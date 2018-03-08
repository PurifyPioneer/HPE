module SLD (SLDTree (..), sld, highestVar, renameVars) where

import Type
import Subst
import Unify

-- | Datastructure repesenting a SLDTree
data SLDTree = Node Goal [(Subst, SLDTree)]
  deriving Show

-- | Creates an sld tree for a given program and request(goal)
sld :: Prog -> Goal -> SLDTree
sld (Prog []) goal           = Node goal []  -- ^ Empty program gives empty tree
sld _         goal@(Goal []) = Node goal []  -- ^ Empty goal gives empty tree
sld prog      goal           =
  -- ^ Run helper with renamed Prog Vars based on the highest Var of our Goal.
  sldHelper (renameVars prog (highestVar goal 0 + 1)) goal
  where
    -- | Helper to use our Prog two times with no recalculation.
    sldHelper :: Prog -> Goal -> SLDTree
    sldHelper prog1 goal1 = Node goal (createBranches prog1 prog1 goal1)

-- | Creates all branches of a SLDTree.
-- Prog(gram) is provided two times. Fist one is the full program.
-- Second is the program we use to work with by removing rules we
-- already tried / used.
createBranches :: Prog -> Prog -> Goal -> [(Subst, SLDTree)]
createBranches _ (Prog []) _         = []  -- ^ Stop if there is no rules left
createBranches _ _         (Goal []) = []  -- ^ There will be no branches
                                           -- without a goal.
-- ^ Create tree branch
-- ^ Get first rule of ruleset when ruleset contains all rules of our
-- program we haven't applied yet.
createBranches fullProg (Prog ((rule :- literals):restRules))
  goal@(Goal (firstGoal:restGoals)) =
    -- ^ Try to unify a rule with a goal
    case unify rule firstGoal of
      -- ^ If rule could not be applied, try to apply rest rules to same goal.
      Nothing  -> createBranches fullProg (Prog restRules) goal
      Just mgu ->
        -- ^ Create first branch with its mgu and Sub-SLDTree
        (mgu, sld fullProg (Goal (map (apply mgu) (literals ++ restGoals)))) :
        -- and append rest branches.
        createBranches fullProg (Prog restRules) goal

-- | Rename the variables in our program so we don't run into a conflict with
-- variables from our goal.
renameVars :: Prog -> Int -> Prog
renameVars (Prog rules) highest = Prog (renameRuleVars rules highest)
  where
    -- | Rename all Vars in a Rule list.
    renameRuleVars :: [Rule] -> Int -> [Rule]
    -- ^ No Rules to rename.
    renameRuleVars []                          _        = []
    renameRuleVars ((term :- literals):restRules) highest1 =
      (
        head (renameTermVars [term] highest1) :-  -- ^ Rename head term.
        (renameTermVars literals highest1)        -- ^ Rename literals.
      ) : renameRuleVars restRules highest1       -- ^ Rename tail of rules
    -- | Rename all Vars in a Term list.
    renameTermVars :: [Term] -> Int -> [Term]
    -- ^ No Terms to rename.
    renameTermVars []                           _        = []
    -- ^ Rename head Var and rest.
    renameTermVars ((Var i):restTerms)          highest1 =
      (Var (i + highest1)):(renameTermVars restTerms highest1)
    -- ^ Rename subterms and rest.
    renameTermVars ((Comb str terms):restTerms) highest1 =
      (Comb str (renameTermVars terms highest1)) :
      (renameTermVars restTerms highest1)

-- | Determines the count of variables in a given goal. Starting with some
-- number (0).
highestVar :: Goal -> Int -> Int
-- ^ for empty goal highest variable is input(ex.: 0)
highestVar (Goal [])                         j = j
-- ^ No need to keep data structure, because we only want the highest var of all
highestVar (Goal ((Comb _ terms):restTerms)) j =
  highestVar (Goal (terms ++ restTerms)) j
-- ^ On variable occurence check if variable index is higher than the highest
-- ^ we found so far
highestVar (Goal ((Var i):restTerms))        j =
  if i > j                                -- ^ Check if we found new max
    then highestVar (Goal restTerms) i    -- ^ Yes: call with new max,restTerms
    else highestVar (Goal restTerms) j    -- ^ No: call with old max,restTerms
