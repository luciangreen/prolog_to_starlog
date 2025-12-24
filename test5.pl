% Test file for additional predicate conversions

% Test is, =
test_is(A, B) :- A is B + 5.
test_equals(A, B) :- A = B.

% Test wrap/2, unwrap/2
test_wrap(A, B) :- wrap(A, B).
test_unwrap(A, B) :- unwrap(A, B).

% Test head/2, tail/2
test_head_tail(A, H, T) :- head(A, H), tail(A, T).

% Test member, delete
test_member_delete(A, X, B) :- member(X, A), delete(A, X, B).

% Test string_to_number/2, random
test_string_number(S, N) :- string_to_number(S, N).
test_random(N, R) :- random(N, R).

% Test length, ceiling, date
test_length(L, N) :- length(L, N).
test_ceiling(X, Y) :- ceiling(X, Y).
test_date(D) :- date(D).

% Test sqrt, round
test_sqrt(X, Y) :- sqrt(X, Y).
test_round(X, Y) :- round(X, Y).

% Test findall, string_from_file
test_findall(X, L) :- findall(N, between(1, X, N), L).
test_string_from_file(F, S) :- string_from_file(F, S).

% Test maplist, sort, intersection
test_maplist(A, B) :- maplist(plus(1), A, B).
test_sort(A, B) :- sort(A, B).
test_intersection(A, B, C) :- intersection(A, B, C).

% Test read_string, atom_string
test_read_string(F, S) :- open(F, read, Stream), read_string(Stream, 1000, _, _, S), close(Stream).
test_atom_string(A, S) :- atom_string(A, S).

% Test call
test_call(X) :- call(member(X, [1, 2, 3])).

% Test complex compression of multiple operations
complex_test(S, N, Result) :-
    string_to_number(S, N),
    sqrt(N, Sqrt),
    ceiling(Sqrt, Ceil),
    round(Ceil, Round),
    random(Round, R),
    number_string(R, Result).

% Test nested compression
nested_test(List, Result) :-
    length(List, Len),
    sort(List, Sorted),
    maplist(sqrt, Sorted, Sqrts),
    reverse(Sqrts, Result).
