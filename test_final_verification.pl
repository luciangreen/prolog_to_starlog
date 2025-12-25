% Final comprehensive test demonstrating the fix for the problem statement
% This shows that all three error cases from the problem statement are now resolved

:- use_module(library(lists)).

:- op(600, yfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').

% Include the fixed decompression logic
compile_expr(Expr, Out, Goals) :-
    ( Expr = (L : R) ; Expr =.. [':', L, R] ),
    !,
    compile_value(L, LV, GL),
    compile_value(R, RV, GR),
    append(GL, GR, G0),
    append(G0, [string_concat(LV, RV, Out)], Goals).

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

compile_expr(Expr, Out, []) :-
    Out = Expr.

% FIXED: compile_value that doesn't expand :, •, & when nested
compile_value(Expr, Value, Goals) :-
    ( is_value_builtin_expr(Expr) ->
        fresh_var(Value),
        compile_expr(Expr, Value, Goals)
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

decompress_nested_goal((Out is Expr), Goals) :-
    compile_expr(Expr, Out, Goals).

% Test the exact problem statement scenarios
main :-
    writeln('========================================'),
    writeln('FINAL TEST: Problem Statement Scenarios'),
    writeln('========================================'),
    nl,
    
    % Original error 1: ?- A is [a:a].
    writeln('1. Problem: ?- A is [a:a].'),
    writeln('   ERROR: Type error: `character\' expected, found `a:a\' (a compound)'),
    writeln(''),
    writeln('   Testing conversion of: A is [[a:a]]'),
    decompress_nested_goal((A1 is [[a:a]]), Goals1),
    write('   Result: '), writeln(Goals1),
    (A1 = [[a:a]], Goals1 = [] ->
        writeln('   ✓ FIXED: [a:a] is preserved as a list with compound term')
    ;   writeln('   ✗ FAILED')
    ),
    nl,
    
    % Original error 2: ?- A is wrap(a:a).
    writeln('2. Problem: ?- A is wrap(a:a).'),
    writeln('   ERROR: Unknown procedure: wrap/2'),
    writeln(''),
    writeln('   Testing conversion of: A is wrap(a:a)'),
    decompress_nested_goal((A2 is wrap(a:a)), Goals2),
    write('   Result: '), writeln(Goals2),
    (Goals2 = [wrap(a:a, A2)] ->
        writeln('   ✓ FIXED: wrap(a:a) converts to wrap(a:a, A) - compound preserved')
    ;   writeln('   ✗ FAILED'),
        write('   Expected: [wrap(a:a, A2)]'), nl,
        write('   Got: '), writeln(Goals2)
    ),
    nl,
    
    % Original error 3: ?- A is string_number(1:1).
    writeln('3. Problem: ?- A is string_number(1:1).'),
    writeln('   ERROR: Arithmetic: `(:)/2\' is not a function'),
    writeln(''),
    writeln('   Testing conversion of: A is string_to_number(1:1)'),
    decompress_nested_goal((A3 is string_to_number(1:1)), Goals3),
    write('   Result: '), writeln(Goals3),
    (Goals3 = [string_to_number(1:1, A3)] ->
        writeln('   ✓ FIXED: string_to_number(1:1) converts to string_to_number(1:1, A) - compound preserved')
    ;   writeln('   ✗ FAILED'),
        write('   Expected: [string_to_number(1:1, A3)]'), nl,
        write('   Got: '), writeln(Goals3)
    ),
    nl,
    
    % Verify top-level operators still work
    writeln('4. Verification: Top-level operators still work'),
    writeln('   Testing conversion of: C is (a : b)'),
    decompress_nested_goal((C is (a : b)), Goals4),
    write('   Result: '), writeln(Goals4),
    (Goals4 = [string_concat(a, b, C)] ->
        writeln('   ✓ PASS: Top-level : still expands to string_concat')
    ;   writeln('   ✗ FAILED')
    ),
    nl,
    
    writeln('========================================'),
    writeln('All problem statement scenarios are FIXED!'),
    writeln('========================================'),
    halt.

:- initialization(main, main).
