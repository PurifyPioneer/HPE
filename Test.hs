module Test where

import Type
import Subst
import Pretty
import Unify
import SLD
--import Parser

-- "append(A, [B|C], [1, 2])"
test1 = pretty (Comb "append" [ Var 0, Comb "." [ Var 1, Var 2 ], Comb "." [ Comb "1" [], Comb "." [ Comb "2" [], Comb "[]" []]]])

-- "{A -> f(C, true), B -> C}"
test2 = pretty (compose (single 1 (Var 2)) (single 0 (Comb "f" [Var 1, Comb "true" []])))

prog = [
    Comb "vater" [ Comb "ingo" [], Comb "thomas" [] ] :- [],
    Comb "vater" [ Comb "ingo" [], Comb "mirko" [] ] :- []
  ]
goal = [
    Comb "vater" [ Comb "ingo" [], Var 0 ]
  ]
test3 = sld (Prog prog) (Goal goal)

term1 = (Comb "append" [Var 0, Var 1, Comb "." [Comb "1" [], Comb "." [Comb "2" [], Comb "[]" []]]])
term2 = (Comb "append" [Comb "." [Var 2,Var 3], (Var 4), Comb "." [Var 2, Var 5]])

testAppend = unify term1 term2
-- TODO: tets agreement set and compose
-- gives nothing

testDS = ds (apply empty term1) (apply empty term2)
-- gives: Just (Var 0,Comb "." [Var 2,Var 3])
-- TODO: ds set seems right check unify.
