% Starlog Nested Operations
% Author: luciangreenPlease
% Date: 2025-07-02 16:23:54 UTC

% Nested string operations
nested_concat(A, B, C, Result) :- 
    Result is ((A : B) : C).

% Nested math operations
nested_math(X, Result) :- 
    Result is ((X + 5) * 2) - 3.

% Nested list operations
process_list(List, Result) :- 
    Len is length_1(List),
    Result is Len * 2.
