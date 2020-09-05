
%Graph representation
vertex(1).
vertex(2).
vertex(3).
vertex(4).
vertex(5).
vertex(6).

connection(1, 2).
connection(1, 3).
connection(2, 4).
connection(2, 5).
connection(3, 5).
connection(4, 1).
connection(5, 2).
connection(5, 4).
connection(6, 3).
connection(6, 4).

color(0).
color(1).
color(2).

setCondition([], _).

setCondition([(S, T) | R], ColorList):-
    member(colorAssigned(S, Color1), ColorList),
    member(colorAssigned(T, Color2), ColorList),
    dif(Color1, Color2), setCondition(R, ColorList).


applyColors([]).

applyColors([colorAssigned(_, C) | Nodes]):-
    color(C), applyColors(Nodes).


main(ColorList):-
    findall((X, Y), connection(X, Y), Connections),
    findall(X, vertex(X), Vertices),
    findall(colorAssigned(X, _),
            member(X, Vertices),
            ColorList),
    setCondition(Connections, ColorList),
    applyColors(ColorList).

