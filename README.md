# Prolog to and from Starlog Converters

* Starlog represents
`append(C,A,D)` as `is(D,&(C,A))`
`string_concat(C,A,D)` as `is(D,:(C,A))`
`atom_concat(C,A,D)` as `is(D,^(C,A))`

* Example Prolog code:
```
greet(Name, Greeting) :-
    string_concat("Hello, ", Name, Temp1),
    string_concat(Temp1, "!", Greeting).
```

* Example equivalent Starlog code:
```
greet(Name, Greeting is ("Hello, " : Name) : "!").
```

* Test using:
`swipl prolog_to_starlog_cli.pl`

* Convert from Starlog to Prolog and back:
`convert_sl_to_pl((Head :- Body), (Head :- NewBody)).`
`convert_pl_to_sl((Head :- Body), (Head :- NewBody)).`
