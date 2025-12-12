% var_utils.pl - Variable renaming utilities for Starlog-Prolog converters
% Date: 2025-07-02 00:25:18 UTC
% Author: luciangreenPlease
% 
% This module provides functions for renaming Prolog variables to human-friendly
% names (A, B, ..., Z, A1, ...) for improved readability.

:- module(var_utils, [rename_vars_pretty/2]).

% Rename variables in a term to pretty names (A, B, C, etc.)
rename_vars_pretty(Term, PrettyTerm) :-
    term_variables(Term, Vars),
    rename_vars(Vars, 0, _),
    copy_term(Term, PrettyTerm).

% Recursively assign pretty names to variables
rename_vars([], _, _).
rename_vars([Var|Vars], N0, N) :-
    N1 is N0 + 1,
    (  N1 =< 26 -> 
       Code is 64 + N1, % A=65, B=66, etc.
       char_code(Letter, Code),
       Var = Letter
    ;  N2 is N1 - 26,
       Code is 64 + (N2 mod 26),
       char_code(Letter, Code), % Cycle through alphabet
       Number is (N2 // 26) + 1,
       atomic_list_concat([Letter, Number], VarName),
       Var = VarName
    ),
    rename_vars(Vars, N1, N).