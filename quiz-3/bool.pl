% A big-step operational semantics for a boolean expression language.

% Define the set of terms.
%
% Note: this is not really necessary, but nice for clarifying exactly
% what you expect to be in the language.
expr(t).
expr(f).
expr(not(E))   :- expr(E).
expr(and(L,R)) :- expr(L), expr(R).
expr(or(L,R))  :- expr(L), expr(R).

% Big-step operational semantics
reduce(t,t).
reduce(f,f).
reduce(not(E),t)   :- reduce(E,f).
reduce(not(E),f)   :- reduce(E,t).
reduce(and(L,R),t) :- reduce(L,t), reduce(R,t).
reduce(and(L,_),f) :- reduce(L,f).
reduce(and(_,R),f) :- reduce(R,f).
reduce(or(L,R),f)  :- reduce(L,f), reduce(R,f).
reduce(or(L,_),t)  :- reduce(L,t).
reduce(or(_,R),t)  :- reduce(R,t).
