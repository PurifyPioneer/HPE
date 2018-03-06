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

--https://stackoverflow.com/questions/15175543/writing-pop-and-push-functions-for-haskell-stack
push :: SLDTree -> [SLDTree] -> [SLDTree]
push sldTree stack = sldTree:stack

pop :: [SLDTree] -> (SLDTree, [SLDTree])
pop [] = ()        -- can't pop emty stack
pop (x:stack) = (x, stack) -- return first stack element and new stack

-- breadth first search
bfs :: Strategy
bfs = bfsHelper empty
  where bfsHelper :: Subst -> Strategy



solve :: Strategy -> Prog -> Goal -> [Subst]
solve strat prog goal = solveHelper (strat (sld prog goal)) goal
  where
    solveHelper :: [Subst] -> Goal -> [Subst]
    solveHelper s goal@(Goal terms) =
      map (\ (Subst subst) ->
        (Subst (filter (\ (index, term) -> varInTerm index terms) subst)
      )) s
