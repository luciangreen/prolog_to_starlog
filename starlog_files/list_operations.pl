% Starlog List Operations
% Author: luciangreenPlease
% Date: 2025-07-02 16:23:54 UTC

% List operations with Starlog notation
first_element([H|_], H).
last_element([X], X).
last_element([_|T], X) :- last_element(T, X).

% List joining
join_lists(A, B, C) :- C is (A & B).

% List length
list_length(List, Len) :- Len is length_1(List).

% Reversed list
reversed_list(List, Reversed) :- Reversed is reverse(List).
