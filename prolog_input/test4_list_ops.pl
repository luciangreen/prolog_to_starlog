% Test4: List operations
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% List operations
first_element([H|_], H).
last_element([X], X).
last_element([_|T], X) :- last_element(T, X).

join_lists(A, B, C) :- append(A, B, C).
list_length(L, N) :- length(L, N).
