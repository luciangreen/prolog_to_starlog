% Test1: Basic facts and rules
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% Simple facts
person(john).
person(susan).
person(mike).

% Simple rules
is_person(X) :- person(X).
two_people(X, Y) :- person(X), person(Y), X \= Y.
