% Test6: Testing compression of nested operations
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% Test compression
nested_concat(A, B, C, Result) :-
    string_concat(A, B, Temp1),
    string_concat(Temp1, C, Result).

nested_math(X, Result) :-
    Y is X + 5,
    Z is Y * 2,
    Result is Z - 3.
    
nested_list(List, Result) :-
    length(List, Len),
    Result is Len * 2.
