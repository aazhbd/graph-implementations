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

connected(yellow, blue).
connected(blue, yellow).
connected(yellow, red).
connected(red, yellow).
connected(blue, red).
connected(red, blue).
