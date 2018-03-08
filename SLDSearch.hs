module SLDSearch (Strategy, dfs, bfs, solve) where

import SLD
import Subst
import Type
import Unify (varInTerm)

-- | Search strategy
type Strategy = SLDTree -> [Subst]

-- | Depth first search
-- follows a branch of the tree until a soloution is found.
-- Possible that no solutions will be found if the stumble into
-- an infinite branch.
dfs :: Strategy
dfs strat = dfsHelper empty strat
  where
    dfsHelper :: Subst -> Strategy
    dfsHelper subst (Node (Goal []) []) = [subst]  -- ^ Found a leaf with
                                                   -- ^ empty goal = success.
    dfsHelper _ (Node _ []) = []  -- ^ Leaf but with non empty goal = fail
    dfsHelper subst (Node goal ((mgu, sldTree):restBranches)) =
      (dfsHelper (compose mgu subst) sldTree) ++
      (dfsHelper subst (Node goal restBranches))  -- ^ goal and subtrees =
                                                  -- ^ traverse subtrees

-- | Breadth first search
-- Looks for solutions by traversing the tree level for level.
-- This will still give solutions even if there are infinite branches.
bfs :: Strategy
bfs tree = workQueue [(empty, tree)]

-- | The subtrees on the same level are put into a queue that
-- will in the next run be used to work on the children of each queue element.
workQueue :: [(Subst,SLDTree)] -> [Subst]
workQueue [] = []
workQueue ((subst,tree):qs) =
  case tree of
    Node (Goal []) []     -> subst : workQueue qs  -- ^ on result get resultList
                                                   -- ^ start looking for next.
    Node _         []     -> workQueue qs          -- ^ on fail start looking
                                                   -- ^ for next right away.
    -- | If a node has node has children they will be saved into a queue with
    -- the substitution that brought us there.
    Node _         kinder -> workQueue (qs ++
                                   (map (\ (substKind, treeKind ) ->
                                   (compose substKind subst, treeKind)) kinder))

-- | Creates an sldTree that is then search with eiter dfs or bfs.
solve :: Strategy -> Prog -> Goal -> [Subst]
solve strat prog goal = solveHelper (strat (sld prog goal)) goal
  where
    -- | Filters the result to only return substitutions for requested goal.
    solveHelper :: [Subst] -> Goal -> [Subst]
    solveHelper s (Goal terms) =
      map (\ (Subst subst) ->
        (Subst (filter (\ (index, _) -> varInTerm index terms) subst)
      )) s
