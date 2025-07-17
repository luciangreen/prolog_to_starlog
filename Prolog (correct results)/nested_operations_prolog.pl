% Starlog Nested Operations
% Author: luciangreenPlease
% Date: 2025-07-02 16:23:54 UTC

% Nested string operations
nested_concat(A, B, C, Result) :- 
    string_concat(A, B, Temp1),
    string_concat(Temp1, C, Result).

% Nested math operations
nested_math(X, Result) :- 
    Temp1 is X + 5,
    Temp2 is Temp1 * 2,
    Result is Temp2 - 3.

% Nested list operations
process_list(List, Result) :- 
    length(List, Len),
    Result is Len * 2.

