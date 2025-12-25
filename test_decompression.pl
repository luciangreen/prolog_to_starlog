% Test the decompression logic directly
:- use_module(library(lists)).

:- op(600, yfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').

% Include the fixed predicates from starlog_to_prolog_cli.pl

% compile_expr predicates
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

% NEW FIXED: compile_value that doesn't expand :, •, & when nested
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

decompress_nested_goal((Out is Expr), Goals) :-
    compile_expr(Expr, Out, Goals).

% Test cases from problem statement
test_all :-
    writeln('=== Testing decompression with compound arguments ==='),
    nl,
    
    % Test 1: wrap(a:a)
    writeln('Test 1: A is wrap(a:a)'),
    decompress_nested_goal((A1 is wrap(a:a)), Goals1),
    write('  Goals: '), writeln(Goals1),
    (Goals1 = [wrap(a:a, A1)] ->
        writeln('  PASS: wrap(a:a) kept as compound argument')
    ;   writeln('  FAIL: wrap(a:a) was incorrectly expanded'),
        write('  Expected: [wrap(a:a, A1)]'), nl,
        write('  Got: '), writeln(Goals1)
    ),
    nl,
    
    % Test 2: string_to_number(1:1)
    writeln('Test 2: A is string_to_number(1:1)'),
    decompress_nested_goal((A2 is string_to_number(1:1)), Goals2),
    write('  Goals: '), writeln(Goals2),
    (Goals2 = [string_to_number(1:1, A2)] ->
        writeln('  PASS: string_to_number(1:1) kept as compound argument')
    ;   writeln('  FAIL: string_to_number(1:1) was incorrectly expanded'),
        write('  Expected: [string_to_number(1:1, A2)]'), nl,
        write('  Got: '), writeln(Goals2)
    ),
    nl,
    
    % Test 3: List with compound [a:a]
    writeln('Test 3: A is [[a:a]]'),
    decompress_nested_goal((A3 is [[a:a]]), Goals3),
    write('  Goals: '), writeln(Goals3),
    (A3 = [[a:a]], Goals3 = [] ->
        writeln('  PASS: [a:a] kept as compound in list')
    ;   writeln('  FAIL: [a:a] was incorrectly expanded'),
        write('  Expected: A3 = [[a:a]], Goals3 = []'), nl,
        write('  Got: A3 = '), write(A3), write(', Goals3 = '), writeln(Goals3)
    ),
    nl,
    
    % Test 4: Top-level : should still expand
    writeln('Test 4: C is (a : b) - top level should expand'),
    decompress_nested_goal((C4 is (a : b)), Goals4),
    write('  Goals: '), writeln(Goals4),
    (Goals4 = [string_concat(a, b, C4)] ->
        writeln('  PASS: Top-level : correctly expands to string_concat')
    ;   writeln('  FAIL: Top-level : not expanded correctly'),
        write('  Expected: [string_concat(a, b, C4)]'), nl,
        write('  Got: '), writeln(Goals4)
    ),
    nl,
    
    writeln('=== All decompression tests completed ===').

main :- test_all, halt.
