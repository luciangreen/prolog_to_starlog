% End-to-end test: Starlog to Prolog conversion with compound arguments
% This simulates the full conversion process

:- use_module(library(lists)).
:- use_module(var_utils).

:- op(600, yfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').

% Test the full conversion pipeline
test_full_conversion :-
    writeln('=== Testing full Starlog to Prolog conversion ==='),
    nl,
    
    % Test case 1: (A is wrap(a:a))
    writeln('Test 1: Converting (A is wrap(a:a))'),
    Starlog1 = (test1(A) :- A is wrap(a:a)),
    convert_clause(Starlog1, Prolog1),
    write('  Starlog: '), writeln(Starlog1),
    write('  Prolog:  '), writeln(Prolog1),
    (Prolog1 = (test1(A) :- wrap(a:a, A)) ->
        writeln('  PASS')
    ;   writeln('  FAIL'),
        write('  Expected: (test1(A) :- wrap(a:a, A))'), nl
    ),
    nl,
    
    % Test case 2: (A is string_to_number(1:1))
    writeln('Test 2: Converting (A is string_to_number(1:1))'),
    Starlog2 = (test2(A) :- A is string_to_number(1:1)),
    convert_clause(Starlog2, Prolog2),
    write('  Starlog: '), writeln(Starlog2),
    write('  Prolog:  '), writeln(Prolog2),
    (Prolog2 = (test2(A) :- string_to_number(1:1, A)) ->
        writeln('  PASS')
    ;   writeln('  FAIL'),
        write('  Expected: (test2(A) :- string_to_number(1:1, A))'), nl
    ),
    nl,
    
    % Test case 3: Top-level concat still works
    writeln('Test 3: Converting (C is (a : b))'),
    Starlog3 = (test3(C) :- C is (a : b)),
    convert_clause(Starlog3, Prolog3),
    write('  Starlog: '), writeln(Starlog3),
    write('  Prolog:  '), writeln(Prolog3),
    (Prolog3 = (test3(C) :- string_concat(a, b, C)) ->
        writeln('  PASS')
    ;   writeln('  FAIL'),
        write('  Expected: (test3(C) :- string_concat(a, b, C))'), nl
    ),
    nl,
    
    writeln('=== Full conversion tests completed ===').

% Simplified conversion logic
convert_clause((Head :- Body), (Head :- PBody)) :-
    convert_body(Body, PBody).
convert_clause(Fact, Fact).

convert_body(Body, PBody) :-
    flatten_conjunction(Body, BodyList),
    maplist(convert_goal, BodyList, PBodyList),
    rebuild_conjunction(PBodyList, PBody).

convert_goal((Out is Expr), PGoal) :-
    !,
    decompress_goal_simple(Expr, Out, PGoal).
convert_goal(Goal, Goal).

decompress_goal_simple(wrap(Arg), Out, wrap(Arg, Out)) :- !.
decompress_goal_simple(string_to_number(Arg), Out, string_to_number(Arg, Out)) :- !.
decompress_goal_simple((L : R), Out, string_concat(L, R, Out)) :- !.
decompress_goal_simple((L & R), Out, append(L, R, Out)) :- !.
decompress_goal_simple((L • R), Out, atom_concat(L, R, Out)) :- !.
decompress_goal_simple(Expr, Out, (Out is Expr)).

flatten_conjunction((A,B), Goals) :- 
    !,
    flatten_conjunction(A, GoalsA),
    flatten_conjunction(B, GoalsB),
    append(GoalsA, GoalsB, Goals).
flatten_conjunction(Goal, [Goal]).

rebuild_conjunction([Goal], Goal) :- !.
rebuild_conjunction([Goal|Goals], (Goal,Rest)) :-
    rebuild_conjunction(Goals, Rest).

main :- test_full_conversion, halt.
