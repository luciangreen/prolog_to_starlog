% Test2: String operations
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% String operations
string_test(A, B, C) :- string_concat(A, B, C).
atom_test(A, B, C) :- atom_concat(A, B, C).
full_name(First, Last, Full) :- 
    string_concat(First, " ", WithSpace),
    string_concat(WithSpace, Last, Full).
