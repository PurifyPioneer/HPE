=(X,X).
member(E,[X|_]) :- =(E,X).
member(E,[_|Xs]) :- member(E,Xs).
