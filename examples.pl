% Example file for testing

fact1.
fact2(a,b).

test_simple(A, B) :- append([1], [2], A), append(A, [3], B).

test_strings(A, B, C) :- 
    atom_concat("Hello", " ", A), 
    atom_concat(A, "world!", B),
    string_length(B, C).

test_builtin(X, Y) :- 
    number_string(42, X),
    string_concat(X, " is the answer", Y).

% More complex example with nested calls
test_nested(Input, Output) :-
    length(Input, Len),
    (Len > 5 -> sort(Input, Output) ; reverse(Input, Output)).

% Test quoted atoms and strings
test_quotes(single_quoted, "double quoted").
