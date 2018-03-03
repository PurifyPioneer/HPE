module Main where

import Type
import Subst
import Pretty
import Unify
import SLD

-- "append(A, [B|C], [1, 2])"
test1 = pretty (Comb "append" [ Var 0, Comb "." [ Var 1, Var 2 ], Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" []]]])

-- "{A -> f(C, true), B -> C}"
test2 = pretty (compose (single 1 (Var 2)) (single 0 (Comb "f" [Var 1, Comb "true" []])))

prog = [
    Comb "vater" [ Comb "Ingo" [], Comb "Thomas" [] ] :- [],
    Comb "vater" [ Comb "Ingo" [], Comb "Mirko" [] ] :- []
  ]
goal = [
    Comb "vater" [ Comb "Ingo" [], Var 0 ]
  ]
test3 = sld (Prog prog) (Goal goal)

main = print "test"



-- pretty listen
