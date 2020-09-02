:- use_module(library(clpfd)). 

vertex(one).
vertex(two).
vertex(three).
vertex(four).
vertex(five).
vertex(six).

connection(one, two).
connection(one, three).
connection(two, four).
connection(two, five).
connection(three, five).
connection(four, one).
connection(five, two).
connection(five, four).
connection(six, three).
connection(six, four).

color(red).
color(green).
color(blue).


createConstraint([], _).

createConstraint([(S, T) | R], ColorList):-
  member(hasColor(S, Color1), ColorList),
  member(hasColor(T, Color2), ColorList),
  dif(Color1, Color2),
  createConstraint(R, ColorList).


colorGraph(ColorList):-
  findall((X, Y), connection(X, Y), Connections),
  findall(X, vertex(X), Vertices),
  findall(hasColor(X, _), member(X, Vertices), ColorList),
  createConstraint(Connections, ColorList),
  applyColors(ColorList).


applyColors([]).

applyColors([hasColor(_, C) | Nodes]):-
  color(C),
  applyColors(Nodes).

