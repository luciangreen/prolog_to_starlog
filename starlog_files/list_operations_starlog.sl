% Starlog List Operations
% Author: luciangreenPlease
% Date: 2025-07-02 16:23:54 UTC
first_element([A|B],A).
last_element([A],A).
last_element([A|B],C):-last_element(B,C).
join_lists(A,B,C):-C is A&B.
list_length(A,B):-B is length_1(A).
reversed_list(A,B):-B is reverse(A).
