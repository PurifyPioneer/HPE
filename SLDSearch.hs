module SLDSearch (Strategy, dfs, bfs, solve) where

import SLD
import Subst
import Type
import Unify (varInTerm)

type Strategy = SLDTree -> [Subst]

-- depth first search
dfs :: Strategy
dfs strat = dfsHelper empty strat
  where
    dfsHelper :: Subst -> Strategy
    dfsHelper subst (Node (Goal []) []) = [subst] -- success
    dfsHelper _ (Node _ []) = []
    dfsHelper subst (Node goal ((mgu, sldTree):restBranches)) =
      (dfsHelper (compose mgu subst) sldTree) ++
      (dfsHelper subst (Node goal restBranches))

-- breadth first search
bfs :: Strategy
bfs (Node goal branches) = []

solve :: Strategy -> Prog -> Goal -> [Subst]
solve strat prog goal = solveHelper (strat (sld prog goal)) goal
  where
    solveHelper :: [Subst] -> Goal -> [Subst]
    solveHelper s goal@(Goal terms) =
      map (\ (Subst subst) ->
        (Subst (filter (\ (index, term) -> varInTerm index terms) subst)
      )) s
