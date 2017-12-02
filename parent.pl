
parent(albert, betsy).
parent(albert, bill).
parent(albert, bob).

parent(alice, bob).
parent(alice, betsy).
parent(alice, bill).

parent(bob, carl).
parent(bob, charlie).

related(X, Y) :-
	parent(X, Y).

related(X, Y) :-
	parent(X, Z),
	related(Z, Y).