% Simple test to demonstrate Starlog to Prolog conversion

test_string_ops(Input1, Input2, Result) :-
    string_concat(Input1, Input2, Result).

test_list_ops(List1, List2, Result) :-
    append(List1, List2, Result).

test_conversion(Input, Output) :-
    number_string(Input, Temp),
    string_length(Temp, Output).
