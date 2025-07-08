% Test Prolog File
% Author: luciangreenPlease
% Date: 2025-07-08
person(john).
person(susan).
person(mike).
is_person(A):-person(A).
double(A,B):-B is A*2.
triple(A,B):-B is A*3.
area_rectangle(A,B,C):-C is A*B.
