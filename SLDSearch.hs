module SLDSearch (dfs, bfs, solve) where

import SLD
import Subst
import Type

-- Tiefensuche
dfs :: Strategy
dfs (Node goal branches) = []

-- Breitensuche
bfs :: Strategy
bfs (Node goal branches) = []

solve :: Strategy -> Prog -> Goal -> [Subst]
solve strat prog goal = []
