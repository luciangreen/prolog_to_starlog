% Direct test of starlog_goal_to_pl conversion
:- use_module(library(lists)).

:- op(600, yfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').

% Test the starlog_goal_to_pl predicate directly
test_direct :-
    writeln('=== Testing starlog_goal_to_pl with compound arguments ==='),
    nl,
    
    % Test 1: wrap with compound argument
    writeln('Test 1: (A is wrap(a:a))'),
    Goal1 = (A is wrap(a:a)),
    (starlog_goal_to_pl(Goal1, Prolog1) ->
        write('  Starlog: '), writeln(Goal1),
        write('  Prolog:  '), writeln(Prolog1),
        (Prolog1 = wrap(a:a, A) ->
            writeln('  PASS: Correctly converted to wrap(a:a, A)')
        ;   writeln('  FAIL: Did not convert to wrap(a:a, A)'),
            write('  Got: '), writeln(Prolog1)
        )
    ;   writeln('  FAIL: starlog_goal_to_pl failed')
    ),
    nl,
    
    % Test 2: string_to_number with compound argument
    writeln('Test 2: (B is string_to_number(1:1))'),
    Goal2 = (B is string_to_number(1:1)),
    (starlog_goal_to_pl(Goal2, Prolog2) ->
        write('  Starlog: '), writeln(Goal2),
        write('  Prolog:  '), writeln(Prolog2),
        (Prolog2 = string_to_number(1:1, B) ->
            writeln('  PASS: Correctly converted to string_to_number(1:1, B)')
        ;   writeln('  FAIL: Did not convert to string_to_number(1:1, B)'),
            write('  Got: '), writeln(Prolog2)
        )
    ;   writeln('  FAIL: starlog_goal_to_pl failed')
    ),
    nl,
    
    % Test 3: Top-level : should still be expanded
    writeln('Test 3: (C is (a : b))'),
    Goal3 = (C is (a : b)),
    (starlog_goal_to_pl(Goal3, Prolog3) ->
        write('  Starlog: '), writeln(Goal3),
        write('  Prolog:  '), writeln(Prolog3),
        (Prolog3 = string_concat(a, b, C) ->
            writeln('  PASS: Correctly converted to string_concat(a, b, C)')
        ;   writeln('  FAIL: Did not convert to string_concat(a, b, C)'),
            write('  Got: '), writeln(Prolog3)
        )
    ;   writeln('  FAIL: starlog_goal_to_pl failed')
    ),
    nl,
    
    writeln('=== All direct tests completed ===').

% Copy the relevant predicates from starlog_to_prolog_cli.pl
starlog_goal_to_pl(X is string_to_number(Y), string_to_number(Y, X)) :- !.
starlog_goal_to_pl(X is wrap(Y), wrap(Y, X)) :- !.
starlog_goal_to_pl(X is (Y : Z), string_concat(Y, Z, X)) :- !, safe_concat_args(Y, Z).

safe_concat_args(Y, Z) :- safe_concat_arg(Y), safe_concat_arg(Z).
safe_concat_arg(Arg) :- var(Arg), !.
safe_concat_arg(Arg) :- atom(Arg), !.
safe_concat_arg(Arg) :- string(Arg), !.
safe_concat_arg('$VAR'(_)) :- !.

main :- test_direct, halt.
