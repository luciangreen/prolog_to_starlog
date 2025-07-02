% Test5: Family relationships
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% Family facts
parent(john, mary).
parent(john, bob).
parent(susan, mary).
parent(susan, bob).
parent(mary, ann).
parent(bob, jim).

% Family rules
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
