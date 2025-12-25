% Test file for compound argument handling
% These should NOT be converted to string_concat operations

test_wrap_compound(A) :- wrap([a:a], A).
test_wrap_compound2(A) :- wrap(a:a, A).
test_string_number_compound(A) :- string_to_number(1:1, A).
