% Test7: Conditional expressions
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% Conditional tests
max(X, Y, X) :- X >= Y.
max(X, Y, Y) :- X < Y.

absolute(X, X) :- X >= 0.
absolute(X, Y) :- X < 0, Y is -X.

even_odd(X, even) :- X mod 2 =:= 0.
even_odd(X, odd) :- X mod 2 =:= 1.
