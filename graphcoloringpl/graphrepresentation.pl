coloring(A,B,C,D,E,F):-
  different(A,B),
  different(A,C),
  different(A,D),
  different(A,F),
  different(B,C),
  different(B,E),
  different(C,D),
  different(C,E),
  different(D,E),
  different(E,F).

different(yellow,blue).
different(blue,yellow).
different(yellow,red).
different(red,yellow).
different(blue,red).
different(red,blue).
