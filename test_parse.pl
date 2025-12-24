:- op(700, xfx, is).
:- op(600, xfx, ':').

test :- write_canonical("Hi, ":Name:", are you"), nl.
