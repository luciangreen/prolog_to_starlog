% Demo Starlog Program - Math and List Operations
% Author: luciangreenPlease
% Date: 2025-07-08
number(1).
number(2).
number(3).
number(4).
number(5).
double(A,B):-B is A*2.
square(A,B):-B is A*A.
add_ten(A,B):-B is A+10.
calculate_area(A,B,C):-C is A*B.
calculate_volume(A,B,C,D):-E is A*B,D is E*C.
greet(A,B):-B is string_concat(Hello, ,A).
is_even(A):-B is A mod 2,B=0.
sequence(A,B):-findall(C,between(1,A,C),B).
demo:-format(=== Starlog Demo ===~n),double(5,A),format(Double of 5 is: ~w~n,[A]),square(4,B),format(Square of 4 is: ~w~n,[B]),calculate_area(10,20,C),format(Area of 10x20 rectangle: ~w~n,[C]),format(Demo complete!~n).
