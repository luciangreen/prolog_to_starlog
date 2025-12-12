:-(test_term_to_atom(A,B),term_to_atom(A,B)).
:-(test_number_string(A,B),number_string(A,B)).
:-(test_maplist(A,B),maplist(atom_chars,A,B)).
:-(test_foldl(A,B),foldl(atom_concat,A,"",B)).
:-(test_string_chars(A,B),string_chars(A,B)).

% Test case for duplicate expression elimination
g(A,B) :- A is f, A is f, B = A.

% Test case for basic conversion and compression
test_compression(A,B,C,D,E) :- 
    string_concat(A,B,C), 
    atom_concat(C,D,E).

% Test case for maintaining quotes
test_quotes(Quoted_atom, "Quoted string", Result) :-
    string_concat(Quoted_atom, "Quoted string", Result).
