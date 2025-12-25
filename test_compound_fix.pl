% Minimal test for compound term handling in Starlog decompression
% This file tests the fix for handling compound terms like a:a inside function arguments

:- use_module(library(lists)).

% Test case 1: wrap with compound argument
test_wrap_compound :-
    % Starlog: B is wrap(a:a)
    % Should convert to: wrap(a:a, B)
    compile_expr(wrap(a:a), B, Goals),
    write('Test wrap(a:a):'), nl,
    write('  Goals: '), writeln(Goals),
    write('  B: '), writeln(B),
    % Check that Goals should be [wrap(a:a, B)]
    (Goals = [wrap(a:a, B)] -> 
        writeln('  PASS: wrap keeps compound argument') 
    ; 
        writeln('  FAIL: wrap incorrectly expanded compound argument')
    ), nl.

% Test case 2: string_to_number with compound argument  
test_string_number_compound :-
    % Starlog: B is string_to_number(1:1)
    % Should convert to: string_to_number(1:1, B)
    compile_expr(string_to_number(1:1), B, Goals),
    write('Test string_to_number(1:1):'), nl,
    write('  Goals: '), writeln(Goals),
    write('  B: '), writeln(B),
    % Check that Goals should be [string_to_number(1:1, B)]
    (Goals = [string_to_number(1:1, B)] -> 
        writeln('  PASS: string_to_number keeps compound argument') 
    ; 
        writeln('  FAIL: string_to_number incorrectly expanded compound argument')
    ), nl.

% Test case 3: Top-level : should still be expanded
test_toplevel_concat :-
    % Starlog: C is (A : B)
    % Should convert to: string_concat(A, B, C)
    compile_expr((a : b), C, Goals),
    write('Test (a : b):'), nl,
    write('  Goals: '), writeln(Goals),
    write('  C: '), writeln(C),
    % Check that Goals should be [string_concat(a, b, C)]
    (Goals = [string_concat(a, b, C)] -> 
        writeln('  PASS: Top-level : correctly expands to string_concat') 
    ; 
        writeln('  FAIL: Top-level : not expanded correctly')
    ), nl.

% Include the necessary predicates from starlog_to_prolog_cli.pl
compile_expr(Expr, Out, Goals) :-
    ( Expr = (L • R) ; Expr =.. ['•', L, R] ),
    !,
    compile_value(L, LV, GL),
    compile_value(R, RV, GR),
    append(GL, GR, G0),
    append(G0, [atom_concat(LV, RV, Out)], Goals).

compile_expr(Expr, Out, Goals) :-
    ( Expr = (L : R) ; Expr =.. [':', L, R] ),
    !,
    compile_value(L, LV, GL),
    compile_value(R, RV, GR),
    append(GL, GR, G0),
    append(G0, [string_concat(LV, RV, Out)], Goals).

compile_expr(Expr, Out, Goals) :-
    ( Expr = (L & R) ; Expr =.. ['&', L, R] ),
    !,
    compile_value(L, LV, GL),
    compile_value(R, RV, GR),
    append(GL, GR, G0),
    append(G0, [append(LV, RV, Out)], Goals).

compile_expr(Expr, Out, Goals) :-
    compound(Expr),
    Expr =.. [F|Args],
    is_value_builtin(F, Arity),
    length(Args, Arity),
    !,
    compile_values(Args, Vals, Gs),
    append(Vals, [Out], ValsWithOut),
    Goal =.. [F|ValsWithOut],
    append(Gs, [Goal], Goals).

compile_expr(Expr, Out, [Out is Expr]) :-
    is_arith_expr(Expr),
    !.

compile_expr(Expr, Out, []) :-
    Out = Expr.

% NEW: compile_value that doesn't expand :, •, & when nested
compile_value(Expr, Value, Goals) :-
    ( is_value_builtin_expr(Expr) ->
        fresh_var(Value),
        compile_expr(Expr, Value, Goals)
    ; is_arith_expr(Expr) ->
        fresh_var(Value),
        Goals = [Value is Expr]
    ;   Value = Expr,
        Goals = []
    ).

is_value_builtin_expr(Expr) :-
    compound(Expr),
    functor(Expr, F, A),
    F \= ':',
    F \= '•',
    F \= '&',
    is_value_builtin(F, A).

compile_values([], [], []).
compile_values([A|As], [V|Vs], Goals) :-
    compile_value(A, V, GA),
    compile_values(As, Vs, GB),
    append(GA, GB, Goals).

fresh_var(V) :- V = _.

is_value_builtin(wrap, 1).
is_value_builtin(string_to_number, 1).
is_value_builtin(sqrt, 1).

is_arith_expr(Expr) :-
    compound(Expr),
    functor(Expr, F, 2),
    memberchk(F, [+, -, *, /, //, mod, **]).

% Run all tests
run_tests :-
    writeln('=== Testing compound term handling in Starlog decompression ==='),
    nl,
    test_wrap_compound,
    test_string_number_compound,
    test_toplevel_concat,
    writeln('=== All tests completed ===').

% Main entry point
main :- run_tests, halt.
