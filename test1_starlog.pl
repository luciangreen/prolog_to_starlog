test_term_to_atom(V0,V1):-V1 is term_to_atom(V0).
test_number_string(V0,V1):-V1 is number_string(V0).
test_maplist(V0,V1):-V1 is maplist(atom_chars,V0).
test_foldl(V0,V1):-V1 is foldl(atom_concat,V0,).
test_string_chars(V0,V1):-V1 is string_chars(V0).
