:- module(prolog_to_starlog_cli, [main/0, hidden_or_special/1]).

% Define operators for Starlog syntax
:- op(600, xfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '…').
:- op(700, xfx, 'is').

main :-
    format('2025-06-14 23:08:55: Starting Prolog to Starlog converter~n'),
    format('2025-06-14 23:08:55: User: luciangreen~n'),
    process_files,
    !.  % Ensure deterministic execution

% Process all Prolog files in current directory
process_files :-
    working_directory(CWD, CWD),
    format('2025-06-14 23:08:55: Working in directory: ~w~n', [CWD]),
    directory_files(CWD, AllFiles),
    exclude(hidden_or_special, AllFiles, Files),
    include(is_source_file, Files, SourceFiles),
    process_prolog_files(SourceFiles),
    !.  % Ensure success

% Filter out hidden and special files
hidden_or_special(File) :-
    atom_concat('.', _, File).
hidden_or_special('..').
hidden_or_special('.').

% Check if file is a source Prolog file
is_source_file(File) :-
    file_name_extension(_Base, 'pl', File),
    \+ sub_atom(File, _, _, 0, '_starlog.pl'),
    \+ sub_atom(File, _, _, 0, '_cli.pl').  % Skip CLI files

% Process Prolog files
process_prolog_files([]).
process_prolog_files([File|Files]) :-
    format('2025-06-14 23:08:55: Processing file: ~w~n', [File]),
    (catch(
        process_single_file(File),
        Error,
        format('Error processing ~w: ~w~n', [File, Error])
    ) ; true),  % Continue even if file fails
    process_prolog_files(Files).

% Process a single Prolog file
process_single_file(File) :-
    read_file_to_clauses(File, Clauses),
    convert_clauses(Clauses, StarlogClauses),
    file_name_extension(Base, _, File),
    atom_concat(Base, '_starlog.pl', OutputFile),
    write_clauses_to_file(OutputFile, StarlogClauses).

% Read file contents into clauses
read_file_to_clauses(File, Clauses) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_clauses(Stream, [], Clauses),
        close(Stream)
    ).

% Read clauses from stream with accumulator
read_clauses(Stream, Acc, Clauses) :-
    repeat,
    catch(
        read_term(Stream, Term, []),
        error(syntax_error(_), _),
        Term = end_of_file
    ),
    (Term == end_of_file ->
        reverse(Acc, Clauses),
        !
    ;
        rename_variables(Term, LetterTerm),
        read_clauses(Stream, [LetterTerm|Acc], Clauses)
    ).

% Convert list of clauses
convert_clauses([], []).
convert_clauses([Clause|Clauses], [StarlogClause|StarlogClauses]) :-
    (convert_pl_to_sl(Clause, StarlogClause) -> true ;
     StarlogClause = Clause),  % Keep original if conversion fails
    convert_clauses(Clauses, StarlogClauses).

% Write clauses to output file
write_clauses_to_file(File, Clauses) :-
    setup_call_cleanup(
        open(File, write, Stream),
        write_clauses(Stream, Clauses),
        close(Stream)
    ).

% Write clauses to stream
write_clauses(_, []).
write_clauses(Stream, [Clause|Clauses]) :-
    catch(
        (write_term(Stream, Clause, [quoted(false), numbervars(true)]),
         write(Stream, '.\n')),
        Error,
        format('Error writing clause: ~w~n', [Error])
    ),
    write_clauses(Stream, Clauses).

% Helper predicate to rename variables to letters
rename_variables(Term, LetterTerm) :-
    copy_term(Term, LetterTerm),  % Make a copy to preserve original
    rename_vars(LetterTerm, 0, _).

rename_vars(Term, N, N1) :-
    var(Term),
    !,
    atom_number(NA, N),
    atom_concat('A', NA, VarName),
    Term = VarName,
    N1 is N + 1.
rename_vars(Term, N, N2) :-
    compound(Term),
    !,
    Term =.. [_|Args],
    rename_var_list(Args, N, N2).
rename_vars(Term, N, N) :-
    atomic(Term).

rename_var_list([], N, N).
rename_var_list([H|T], N, N2) :-
    rename_vars(H, N, N1),
    rename_var_list(T, N1, N2).

% Conversion predicates
convert_pl_to_sl((Head :- Body), (Head :- NewBody)) :-
    !,
    convert_body_pl_to_sl(Body, NewBody).
convert_pl_to_sl(Term, Term).

% Convert body predicates
convert_body_pl_to_sl((A, B), (NewA, NewB)) :-
    !,
    convert_body_pl_to_sl(A, NewA),
    convert_body_pl_to_sl(B, NewB).

% Handle special cases for string operations
convert_body_pl_to_sl(string_concat(A, B, C), (C is A : B)) :- !.
convert_body_pl_to_sl(append(A, B, C), (C is A & B)) :- !.
convert_body_pl_to_sl(atom_concat(A, B, C), (C is A … B)) :- !.

% Handle 'is' expressions directly
convert_body_pl_to_sl((Output is Expr), (Output is Expr)) :- !.

% Handle built-in predicates
convert_body_pl_to_sl(Term, (Output is NewTerm)) :-
    compound(Term),
    built_in_predicate(Term, Output, NewTerm),
    !.

% Keep non-built-in predicates unchanged
convert_body_pl_to_sl(Term, Term).

% Built-in predicate handlers
built_in_predicate(term_to_atom(Term, Result), Result, term_to_atom(Term)).
built_in_predicate(atom_to_term(Atom, Result), Result, atom_to_term(Atom)).
built_in_predicate(string_to_atom(String, Result), Result, string_to_atom(String)).
built_in_predicate(atom_to_string(Atom, Result), Result, atom_to_string(Atom)).
built_in_predicate(number_chars(Number, Result), Result, number_chars(Number)).
built_in_predicate(atom_chars(Atom, Result), Result, atom_chars(Atom)).
built_in_predicate(string_chars(String, Result), Result, string_chars(String)).
built_in_predicate(sub_atom(Atom, Before, Length, After, Result), Result, sub_atom(Atom, Before, Length, After)).
built_in_predicate(sub_string(String, Before, Length, After, Result), Result, sub_string(String, Before, Length, After)).
built_in_predicate(atom_length(Atom, Result), Result, atom_length(Atom)).
built_in_predicate(string_length(String, Result), Result, string_length(String)).
built_in_predicate(maplist(Pred, List, Result), Result, maplist(Pred, List)).
built_in_predicate(foldl(Pred, List, Init, Result), Result, foldl(Pred, List, Init)).
built_in_predicate(number_string(Number, Result), Result, number_string(Number)).