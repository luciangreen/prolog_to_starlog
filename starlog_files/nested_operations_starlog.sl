% Starlog Nested Operations
% Author: luciangreenPlease
% Date: 2025-07-02 16:23:54 UTC
nested_concat(A,B,C,D):-D is (A:B):C.
nested_math(A,B):-B is (A+5)*2-3.
process_list(A,B):-C is length_1(A),B is C*2.
