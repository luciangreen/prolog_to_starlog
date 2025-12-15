:- consult(starlog_to_prolog_cli).
:- consult(var_utils).

test_clause(Input, Output) :-
    numbervars(Input, 0, _),
    starlog_to_prolog_cli:starlog_to_pl_with_decompression(Input, Output).

test_all :-
    % Test 1: A is B+5
    test_clause((test_is(A,B):-A is B+5), R1),
    format('Test 1 - A is B+5: ~w~n', [R1]),
    
    % Test 2: member
    test_clause((test_member(A):-member(A,[1,2,3])), R2),
    format('Test 2 - member: ~w~n', [R2]),
    
    % Test 3: A is f (atom)
    test_clause((g(A,B):-A is f,B=A), R3),
    format('Test 3 - A is f: ~w~n', [R3]),
    
    % Test 4: B is wrap(A)
    test_clause((test_wrap(A,B):-B is wrap(A)), R4),
    format('Test 4 - B is wrap(A): ~w~n', [R4]),
    
    % Test 5: A is member_1(B) 
    test_clause((test_member_delete(A,B,C):-A is member_1(B),C is delete(A,B)), R5),
    format('Test 5 - A is member_1(B): ~w~n', [R5]),
    
    % Test 6: B is read_string(C,1000,D,E)
    format('About to test read_string...~n'),
    catch(
        test_clause((test_read_string(A,B):-open(A,read,C),B is read_string(C,1000,D,E),close(C)), R6),
        Error,
        (format('Error in test 6: ~w~n', [Error]), R6 = error)
    ),
    format('Test 6 - read_string: ~w~n', [R6]),
    
    true.

:- initialization(test_all).
