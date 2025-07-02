% Test9: Recursive predicates
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% Recursive predicates
factorial(0, 1).
factorial(N, F) :- 
    N > 0, 
    N1 is N - 1, 
    factorial(N1, F1), 
    F is N * F1.

fibonacci(0, 0).
fibonacci(1, 1).
fibonacci(N, F) :- 
    N > 1, 
    N1 is N - 1, 
    N2 is N - 2, 
    fibonacci(N1, F1), 
    fibonacci(N2, F2), 
    F is F1 + F2.

sum_list([], 0).
sum_list([H|T], Sum) :- 
    sum_list(T, TailSum), 
    Sum is H + TailSum.
