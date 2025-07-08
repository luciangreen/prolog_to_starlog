% Simple Starlog Test  
% Author: luciangreenPlease
% Date: 2025-07-08

% Basic facts
person(john).
age(john, 25).

% Starlog-style rule using 'is' operator
get_age(Person, Age) :- Age is age(Person).