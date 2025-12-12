% Test file for special notation
foo.
bar(X) :- baz(X).
baz(a).

% Test trailing white space removal
remove_trailing_white_space(F4,F8) :-
    reverse(F4,F6),
    findall_until_fail(B,member(B,F6),is_space(B),F5),
    append(F5,F7,F6),
    reverse(F7,F8),!.

% Test split12 function
split12([],_,A,A):-!.
split12(Q2,L16,L20,L17) :-
	length(L18,L16),
	append(L18,L19,Q2),
	append(L20,[L18],L21),
	split12(L19,L16,L21,L17),!.
split12(A,_L16,B,C) :- append(B,[A],C),!.
