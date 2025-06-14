# Prolog to and from Starlog Converters

* Starlog represents
`append(C,A,D)` as `is(D,&(C,A))`
`string_concat(C,A,D)` as `is(D,:(C,A))`
`atom_concat(C,A,D)` as `is(D,^(C,A))`

* Example Prolog code:
```
:-(greet(A,B),','(string_constant(C),','(string_concat(C,A,D),','(string_constant(E),string_concat(D,E,B))))).
join_lists([],A,A).
:-(join_lists([A|B],C,[A|D]),join_lists(B,C,D)).
person(john).
person(jane).
person(bob).
:-(likes(john,A),','(person(A),\+(=(A,john)))).
likes(jane,bob).
:-(format_greeting(A,B),','(string_constant(C),','(string_concat(C,A,D),','(string_constant(E),string_concat(D,E,B))))).
```

* Example equivalent Starlog code:
```
:-(greet(A,B),','(string_constant(C),','(is(D,:(C,A)),','(string_constant(E),is(B,:(D,E)))))).
join_lists([],A,A).
:-(join_lists([A|B],C,[A|D]),join_lists(B,C,D)).
person(john).
person(jane).
person(bob).
:-(likes(john,A),','(person(A),\+(=(A,john)))).
likes(jane,bob).
:-(format_greeting(A,B),','(string_constant(C),','(is(D,:(C,A)),','(string_constant(E),is(B,:(D,E)))))).
```

* Test using:
`swipl prolog_to_starlog_cli.pl`

* Convert from Starlog to Prolog and back:
`convert_sl_to_pl((Head :- Body), (Head :- NewBody)).`
`convert_pl_to_sl((Head :- Body), (Head :- NewBody)).`
