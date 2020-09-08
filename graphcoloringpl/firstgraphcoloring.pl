
% first attempt simple graph coloring without neighbors having the same color.

coloring(V1, V2, V3, V4, V5, V6):-
  connected(V1, V2),
  connected(V1, V3),
  connected(V2, V4),
  connected(V2, V5),
  connected(V3, V5),
  connected(V4, V1),
  connected(V5, V2),
  connected(V5, V4),
  connected(V6, V3),
  connected(V6, V4).

connected(C1, C2):-
    color(C1), color(C2), (C1 \= C2).

color(C):-
    member(C, [0, 1, 2, 3]).
