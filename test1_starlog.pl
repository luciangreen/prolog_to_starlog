test_term_to_atom(A0,A1):-A1 is term_to_atom(A0).
test_number_string(A0,A1):-A1 is number_string(A0).
test_maplist(A0,A1):-A1 is maplist(atom_chars,A0).
test_foldl(A0,A1):-A1 is foldl(atom_concat,A0,).
test_string_chars(A0,A1):-A1 is string_chars(A0).
