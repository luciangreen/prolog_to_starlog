% Starlog List Operations
% Author: luciangreenPlease
% Date: 2025-07-02 16:23:54 UTC

% List operations with Starlog notation
first_element([H|_], H).

last_element([X], X).

last_element([_|T], X) :- 
    last_element(T, X).

% List joining
join_lists(A, B, C) :- 
    append(A, B, C).

% List length
list_length(List, Len) :- 
    length(List, Len).

% Reversed list
reversed_list(List, Reversed) :- 
    reverse(List, Reversed).

