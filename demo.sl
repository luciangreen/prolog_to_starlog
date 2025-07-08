% Demo Starlog Program - Math and List Operations
% Author: luciangreenPlease
% Date: 2025-07-08

% Basic facts
number(1).
number(2).
number(3).
number(4).
number(5).

% Starlog-style arithmetic operations
double(X, Result) :- Result is X * 2.
square(X, Result) :- Result is X * X.
add_ten(X, Result) :- Result is X + 10.

% More complex calculations
calculate_area(Width, Height, Area) :-
    Area is Width * Height.

calculate_volume(Width, Height, Depth, Volume) :-
    Area is Width * Height,
    Volume is Area * Depth.

% String operations
greet(Name, Greeting) :-
    Greeting is string_concat("Hello, ", Name).

% Check if number is even
is_even(X) :-
    Remainder is X mod 2,
    Remainder = 0.

% Generate sequence
sequence(N, List) :-
    findall(X, between(1, N, X), List).

% Main demo predicate
demo :-
    format('=== Starlog Demo ===~n'),
    double(5, D),
    format('Double of 5 is: ~w~n', [D]),
    square(4, S),
    format('Square of 4 is: ~w~n', [S]),
    calculate_area(10, 20, Area),
    format('Area of 10x20 rectangle: ~w~n', [Area]),
    format('Demo complete!~n').