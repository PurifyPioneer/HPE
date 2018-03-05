%! vater(vater,kind)
vater(klaus, herbert).
vater(rudolf, obama).
vater(manni, thomas).
vater(manni, obama).
vater(obama, lincoln).
%! grossvater(grossvater, enkel)
grossvater(X, Z) :- vater(X, Y), vater(Y, Z).

%! p(X,Z) :- q(X,Y), p(Y,Z).
%! p(X,X).
%! q(a,b).

%! Grossvater anfrage. Variablen werden nicht (richtig) umbenannt.
%!
%! grossvater(manni,X)
%! Kopf: Goal ist Anfrage "grossvater" (mit Variable "manni"(index 0))                                                   |
%! Node (Goal [Comb "grossvater" [Comb "manni" [],Var 0]])
%!                              Kante: [Variable mit index 0 aus programm X wird mit variable mit index 0 aus anfrage "manni" substituiert]
%!                                     hier Fehler weil in zweitem Teil auch mit "manni ersetzt wird .. offset Flasch ??"
%!                              [(Subst [(0,Comb "manni" []),(1,Comb "manni" [])],
%!                              Node (Goal [Comb "vater" [Comb "manni" [],Var2],Comb "vater" [Var 2,Comb "manni" []]]) [])]
%!
%! vater(manni,X).
%! Node (Goal [Comb "vater" [Comb "manni" [],Var 0]]) [(Subst [(0,Comb "thomas" [])],Node (Goal []) []),(Subst [(0,Comb "obama" [])],Node (Goal []) [])]
