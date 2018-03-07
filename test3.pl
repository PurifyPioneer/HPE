% Vereine
verein(kiel).
verein(leipzig).
verein(melsungen).
verein(berlin).

% Positionen
position(rl).
position(tw).
position(rm).

% Spieler
spieler(wolff).
spieler(heinevetter).
spieler(drux).
spieler(kuehn).
spieler(weber).

% Spielt-Für-Relation
spieltFuer(heinevetter, berlin).
spieltFuer(wolff, kiel).
spieltFuer(drux, berlin).
spieltFuer(kuehn, melsungen).
spieltFuer(weber, leipzig).

% Spielt-auf-Position-Relation
spieltPosition(heinevetter, tw).
spieltPosition(wolff, tw).
spieltPosition(drux, rl).
spieltPosition(kuehn, rl).
spieltPosition(weber, rm).
spieltPosition(weber, rl).

% Auswechseln-Relation
kannAuswechseln(Spieler1, Spieler2) :- spieltPosition(Spieler1, Pos), spieltPosition(Spieler2, Pos).
% Falls Wolff nicht ausgewechselt werden soll, kann man noch 'Spieler1 \= wolff' ergänzen.

% Spielen-zusammen-Relation
spielenZusammen(Spieler1, Spieler2) :-
  spieltFuer(Spieler1, Verein), spieltFuer(Spieler2, Verein).

% Spielen-gegeneinander-Relation
spielenGegeneinander(Spieler1, Spieler2) :-
  spieltFuer(Spieler1, Verein1), spieltFuer(Spieler2, Verein2).
