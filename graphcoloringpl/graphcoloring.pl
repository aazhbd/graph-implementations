:- use_module(library(clpfd)). 

vertex(one).
vertex(two).
vertex(three).
vertex(four).
vertex(five).

edge(one, two).
edge(two, three).
edge(one, three).
edge(two, one).
edge(three, one).
edge(four, one).
edge(four, two).
edge(three, five).
edge(five, one).
edge(five, four).

color(one).
color(two).
color(three).


colorGraph(ColorList) :- 
  findall((X, Y), edge(X, Y), Edges),
  findall(X, vertex(X), Vertexes),
  findall(hasColor(X, _), member(X, Vertexes), ColorList),
  createConstraint(Edges,ColorList),
  colorNodes(ColorList).

createConstraint([],_).

createConstraint([(V1,V2)|RL],ColorList):-
  member(hasColor(V1,C1),ColorList),
  member(hasColor(V2,C2),ColorList),
  dif(C1,C2),
  createConstraint(RL,ColorList).

colorNodes([]).

colorNodes([hasColor(_,C)|Nodes]) :-
  color(C),
  colorNodes(Nodes).


