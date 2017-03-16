% Define the set of all natural numbers.
nat(zero).
nat(succ(X)) :- nat(X).

% Define some named predicates to make referring to numbers easier.
one(succ(zero)).
two(succ(X))   :- one(X).
three(succ(X)) :- two(X).
four(succ(X))  :- three(X).
five(succ(X))  :- four(X).
six(succ(X))   :- five(X).

% Addition.
plus(zero,Y,Y).
plus(succ(X),Y,succ(Z)) :- plus(X,Y,Z).

% Less than or equal to.
lte(zero,zero).
lte(succ(_),zero).
lte(succ(X),succ(Y)) :- lte(X,Y).

% Subtraction.
minus(X,Y,Z) :- plus(Y,Z,X).

% Multiplication.
times(zero,_,zero).
times(succ(X),Y,Z) :- plus(T,Y,Z), times(X,Y,T).

% Division.
divide(X,Y,Z) :- times(Y,Z,X).

% Sum of the numbers in a list.
sum([], zero).
sum([H|T], N1) :- plus(H,N2,N1), sum(T,N2).
