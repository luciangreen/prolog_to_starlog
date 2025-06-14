% test using
% swipl prolog_to_starlog_cli.pl

% convert_sl_to_pl((Head :- Body), (Head :- NewBody)).
% convert_pl_to_sl((Head :- Body), (Head :- NewBody)).

:- module(prolog_to_starlog_cli, [main/0]).

:- use_module(library(readutil)).

% Define operators for Starlog syntax
:- op(600, xfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '^').
:- op(700, xfx, 'is').

% Debug predicate with timestamp
debug_msg(Format) :-
    debug_msg(Format, []).

debug_msg(Format, Args) :-
    format('2025-06-14 07:18:45: ', []),
    format(Format, Args),
    nl,
    flush_output.

% Helper predicates for file existence
ensure_file(File) :-
    (exists_file(File) ->
        true
    ;
        open(File, write, Stream),
        close(Stream)).

% String constants
string_constant("Hello, ").
string_constant("!").
string_constant("Greetings ").
string_constant(", welcome!").
string_constant(" (").
string_constant(") since ").
string_constant(")").
string_constant("/").

% Write test file 1 with proper string handling
write_test_file1 :-
    debug_msg('Writing test file 1'),
    ensure_file('test1.pl'),
    setup_call_cleanup(
        open('test1.pl', write, Stream, [encoding(utf8)]),
        write_test1_content(Stream),
        close(Stream)
    ).

% Write test file 1 content
write_test1_content(Stream) :-
    forall(test1_term(Term),
           (write_canonical_term(Stream, Term),
            write(Stream, '.\n'))).

% Test file 1 terms
test1_term((greet(Name, Greeting) :-
    string_constant(Hello),
    string_concat(Hello, Name, Temp),
    string_constant(Bang),
    string_concat(Temp, Bang, Greeting))).
test1_term((join_lists([], L, L))).
test1_term((join_lists([H|T], L, [H|Result]) :-
    join_lists(T, L, Result))).
test1_term(person(john)).
test1_term(person(jane)).
test1_term(person(bob)).
test1_term((likes(john, X) :- person(X), \+ X = john)).
test1_term(likes(jane, bob)).
test1_term((format_greeting(Name, Greeting) :-
    string_constant(Greet),
    string_concat(Greet, Name, Temp),
    string_constant(Welcome),
    string_concat(Temp, Welcome, Greeting))).

% Write test file 2 with proper string handling
write_test_file2 :-
    debug_msg('Writing test file 2'),
    ensure_file('test2.pl'),
    setup_call_cleanup(
        open('test2.pl', write, Stream, [encoding(utf8)]),
        write_test2_content(Stream),
        close(Stream)
    ).

% Write test file 2 content
write_test2_content(Stream) :-
    forall(test2_term(Term),
           (write_canonical_term(Stream, Term),
            write(Stream, '.\n'))).

% Test file 2 terms
test2_term((make_file_path(Dir, SubDir, File, Path) :-
    string_constant(Slash),
    atom_concat(Dir, Slash, T1),
    atom_concat(T1, SubDir, T2),
    atom_concat(T2, Slash, T3),
    atom_concat(T3, File, Path))).
test2_term((process_lists(List1, List2, Str1, Str2, Result) :-
    append(List1, List2, Combined),
    string_concat(Str1, Str2, CombinedStr),
    append(Combined, [CombinedStr], Result))).
test2_term(employee(john, developer, 2020)).
test2_term(employee(jane, manager, 2018)).
test2_term(employee(bob, tester, 2021)).
test2_term((senior_employee(Person, Role, Year) :-
    employee(Person, Role, Year),
    Year < 2020)).
test2_term((format_employee(Name, Role, Year, Info) :-
    string_constant(OpenParen),
    string_concat(Name, OpenParen, T1),
    string_concat(T1, Role, T2),
    string_constant(SincePart),
    string_concat(T2, SincePart, T3),
    string_concat(T3, Year, T4),
    string_constant(CloseParen),
    string_concat(T4, CloseParen, Info))).
test2_term((filter_recent_employees([], _, []))).
test2_term((filter_recent_employees([employee(Name,_Role,Year)|Rest], Threshold, [Name|Names]) :-
    Year >= Threshold,
    filter_recent_employees(Rest, Threshold, Names))).
test2_term((filter_recent_employees([_|Rest], Threshold, Names) :-
    filter_recent_employees(Rest, Threshold, Names))).

% Write a term in canonical form
write_canonical_term(Stream, Term) :-
    \+ \+ ( % Double negation to preserve variable names
        numbervars(Term, 0, _),
        write_term(Stream, Term, [
            quoted(true),
            ignore_ops(true),
            numbervars(true)
        ])
    ).

% Read terms from file
read_terms(File, Terms) :-
    debug_msg('Reading terms from: ~w', [File]),
    (exists_file(File) ->
        setup_call_cleanup(
            open(File, read, Stream, [encoding(utf8)]),
            read_all_terms(Stream, Terms),
            close(Stream)
        )
    ;
        debug_msg('File not found: ~w', [File]),
        Terms = []
    ).

% Read all terms
read_all_terms(Stream, Terms) :-
    read_term(Stream, Term, [
        variable_names(_),
        double_quotes(string)
    ]),
    (Term == end_of_file ->
        Terms = []
    ;
        debug_msg('Read term: ~w', [Term]),
        Terms = [Term|Rest],
        read_all_terms(Stream, Rest)
    ).

% Convert between Prolog and Starlog
convert_pl_to_sl((Head :- Body), (Head :- NewBody)) :- !,
    convert_body_pl_to_sl(Body, NewBody).
convert_pl_to_sl(Term, Term).

convert_sl_to_pl((Head :- Body), (Head :- NewBody)) :- !,
    convert_body_sl_to_pl(Body, NewBody).
convert_sl_to_pl(Term, Term).

% Convert Prolog body to Starlog
convert_body_pl_to_sl((A, B), (NewA, NewB)) :- !,
    convert_body_pl_to_sl(A, NewA),
    convert_body_pl_to_sl(B, NewB).
convert_body_pl_to_sl(string_concat(A, B, C), (C is A : B)) :- !.
convert_body_pl_to_sl(append(A, B, C), (C is A & B)) :- !.
convert_body_pl_to_sl(atom_concat(A, B, C), (C is A ^ B)) :- !.
convert_body_pl_to_sl(\+(A), \+(NewA)) :- !,
    convert_body_pl_to_sl(A, NewA).
convert_body_pl_to_sl(Term, Term).

% Convert Starlog body to Prolog
convert_body_sl_to_pl((A, B), (NewA, NewB)) :- !,
    convert_body_sl_to_pl(A, NewA),
    convert_body_sl_to_pl(B, NewB).
convert_body_sl_to_pl((C is A : B), string_concat(A, B, C)) :- !.
convert_body_sl_to_pl((C is A & B), append(A, B, C)) :- !.
convert_body_sl_to_pl((C is A ^ B), atom_concat(A, B, C)) :- !.
convert_body_sl_to_pl(\+(A), \+(NewA)) :- !,
    convert_body_sl_to_pl(A, NewA).
convert_body_sl_to_pl(Term, Term).

% Process files
process_files(InputFile, OutputFile, Converter) :-
    debug_msg('Processing files: Input=~w, Output=~w', [InputFile, OutputFile]),
    ensure_file(OutputFile),
    read_terms(InputFile, Terms),
    maplist(Converter, Terms, ConvertedTerms),
    write_terms(OutputFile, ConvertedTerms).

% Write terms to file
write_terms(File, Terms) :-
    debug_msg('Writing terms to: ~w', [File]),
    setup_call_cleanup(
        open(File, write, Stream, [encoding(utf8)]),
        write_terms_to_stream(Stream, Terms),
        close(Stream)
    ).

% Write terms to stream
write_terms_to_stream(Stream, Terms) :-
    forall(member(Term, Terms),
           (debug_msg('Writing term: ~w', [Term]),
            write_canonical_term(Stream, Term),
            write(Stream, '.\n'))).

% Round-trip testing
test_round_trip :-
    debug_msg('Running round-trip tests...'),
    write_test_files,
    run_conversions,
    check_results,
    !.  % Cut to prevent backtracking

% Write test files
write_test_files :-
    debug_msg('Creating test files...'),
    write_test_file1,
    write_test_file2.

% Run all conversions
run_conversions :-
    debug_msg('Running conversions...'),
    ensure_file('test1.sl'),
    ensure_file('test2.sl'),
    ensure_file('test1_rt.pl'),
    ensure_file('test2_rt.pl'),
    process_files('test1.pl', 'test1.sl', convert_pl_to_sl),
    process_files('test2.pl', 'test2.sl', convert_pl_to_sl),
    process_files('test1.sl', 'test1_rt.pl', convert_sl_to_pl),
    process_files('test2.sl', 'test2_rt.pl', convert_sl_to_pl).

% Check results
check_results :-
    debug_msg('Checking results...'),
    check_file_pair('test1.pl', 'test1_rt.pl'),
    check_file_pair('test2.pl', 'test2_rt.pl').

% Check file pair
check_file_pair(File1, File2) :-
    read_file_to_string(File1, Content1, [encoding(utf8)]),
    (exists_file(File2) ->
        read_file_to_string(File2, Content2, [encoding(utf8)]),
        compare_contents(File1, File2, Content1, Content2)
    ;
        debug_msg('~w does not exist', [File2])
    ).

% Compare contents
compare_contents(File1, File2, Content1, Content2) :-
    normalize_content(Content1, Norm1),
    normalize_content(Content2, Norm2),
    (Norm1 = Norm2 ->
        debug_msg('~w and ~w match', [File1, File2])
    ;
        debug_msg('~w and ~w differ:', [File1, File2]),
        debug_msg('Original:~n~w', [Content1]),
        debug_msg('Round-trip:~n~w', [Content2])
    ).

% Normalize content
normalize_content(Content, Normalized) :-
    split_string(Content, "\n", "", Lines),
    exclude(empty_line, Lines, NonEmpty),
    atomic_list_concat(NonEmpty, '\n', Normalized).

% Check for empty line
empty_line("").

% Main entry point
main :-
    debug_msg('Starting program'),
    catch(
        test_round_trip,
        Error,
        (debug_msg('Error: ~w', [Error]), fail)
    ),
    !.  % Cut to prevent backtracking

% Initialize
:- initialization(main, main).