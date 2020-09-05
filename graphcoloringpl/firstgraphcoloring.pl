
% first attempt simple graph coloring without neighbors having the same color.

coloring(A, B, C, D, E, F):-
  connected(A, B),
  connected(A, C),
  connected(A, D),
  connected(A, F),
  connected(B, C),
  connected(B, E),
  connected(C, D),
  connected(C, E),
  connected(D, E),
  connected(E, F).


connected(X, Y):-
    color(X), color(Y), (X \= Y).

color(X):-
    member(X, [red, blue, green, yellow]).


