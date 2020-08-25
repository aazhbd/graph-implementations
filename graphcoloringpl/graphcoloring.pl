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


colorGraph(ColorList) :- 
  findall((X, Y), connection(X, Y), Connections),
  findall(X, vertex(X), Vertices),
  findall(hasColor(X, _), member(X, Vertices), ColorList),
  createConstraint(Connections, ColorList),
  applyColors(ColorList).

createConstraint([],_).

createConstraint([(V1,V2)|RL],ColorList):-
  member(hasColor(V1,C1),ColorList),
  member(hasColor(V2,C2),ColorList),
  dif(C1,C2),
  createConstraint(RL,ColorList).

applyColors([]).

applyColors([hasColor(_,C)|Nodes]) :-
  color(C),
  applyColors(Nodes).


