:- use_module(var_utils).

% Define operators for Starlog syntax - order matters!
:- op(700, xfx, is).
:- op(600, xfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').

main :-
    convert_all_starlog_to_prolog,
    halt.

convert_all_starlog_to_prolog :-
    working_directory(CWD, CWD),
    format('~n=== Starlog to Prolog Converter ===~n'),
    format('Date/Time: 2025-07-02 16:23:54 UTC~n'),
    format('User: luciangreenPlease~n'),
    format('Working in directory: ~w~n', [CWD]),
    
    % Look for Starlog files in the current directory
    directory_files('.', Files),
    exclude(hidden_or_special, Files, VisibleFiles),
    include(is_prolog_file, VisibleFiles, PrologFiles),
    ( PrologFiles = [] ->
        format('No .pl files found for conversion~n')
    ; length(PrologFiles, NumFiles),
      format('Found ~w files to process: ~w~n', [NumFiles, PrologFiles]),
      process_starlog_files(PrologFiles)
    ),
    format('~n=== Starlog to Prolog Conversion Complete ===~n').

hidden_or_special(File) :- sub_atom(File, 0, 1, _, '.').
hidden_or_special('..').
hidden_or_special('.').

is_prolog_file(File) :-
    (file_name_extension(_, 'pl', File) ; file_name_extension(_, 'sl', File)),
    File \= 'var_utils.pl',
    File \= 'starlog_to_prolog.pl',
    File \= 'prolog_to_starlog.pl',
    File \= 'file_extension_converter.pl'.

process_starlog_files([]).
process_starlog_files([File|Files]) :-
    format('Processing file: ~w~n', [File]),
    catch(
        (process_single_file(File), format('Successfully processed ~w~n', [File])),
        Error,
        format('Error processing ~w: ~w~n', [File, Error])
    ),
    process_starlog_files(Files).

process_single_file(InputFile) :-
    format('Reading file: ~w~n', [InputFile]),
    read_file_to_clauses(InputFile, Clauses0),
    length(Clauses0, NumRaw),
    format('Read ~w raw clauses from ~w~n', [NumRaw, InputFile]),
    include(is_clause, Clauses0, Clauses),
    length(Clauses, NumFiltered),
    format('Filtered to ~w valid clauses~n', [NumFiltered]),
    maplist(starlog_to_pl, Clauses, PrologClauses),
    length(PrologClauses, NumConverted),
    format('Converted ~w clauses to Prolog~n', [NumConverted]),
    maplist(rename_vars_pretty, PrologClauses, PrettyClauses),
    
    % Extract base filename and create output path
    file_name_extension(Base, Ext, InputFile),
    ( Ext = 'sl' ->
        atom_concat(Base, '_prolog.pl', OutputFile)
    ; atom_concat(Base, '_prolog.pl', OutputFile)
    ),
    
    % Copy original file header if it exists
    copy_file_header(InputFile, OutputFile),
    
    format('Writing to: ~w~n', [OutputFile]),
    write_clauses_to_file(OutputFile, PrettyClauses),
    format('Wrote converted file: ~w~n', [OutputFile]).

% Copy header comments from source file to destination file
copy_file_header(SourceFile, DestFile) :-
    setup_call_cleanup(
        open(SourceFile, read, InStream),
        (
            setup_call_cleanup(
                open(DestFile, write, OutStream),
                copy_header_comments(InStream, OutStream),
                close(OutStream)
            )
        ),
        close(InStream)
    ).

copy_header_comments(InStream, OutStream) :-
    read_line_to_string(InStream, Line),
    (   Line \= end_of_file,
        sub_atom(Line, 0, 1, _, '%')
    ->  format(OutStream, "~s~n", [Line]),
        copy_header_comments(InStream, OutStream)
    ;   true
    ).

is_clause((:- _)) :- !, fail.
is_clause((?- _)) :- !, fail.
is_clause(_) :- !.

read_file_to_clauses(File, Clauses) :-
    setup_call_cleanup(
        open(File, read, Stream),
        read_clauses(Stream, [], Clauses),
        close(Stream)
    ).

read_clauses(Stream, Acc, Clauses) :-
    read_term(Stream, Term, []),
    (   Term == end_of_file
    ->  reverse(Acc, Clauses)
    ;   read_clauses(Stream, [Term|Acc], Clauses)
    ).

% --- Starlog clause to Prolog clause converter ---
starlog_to_pl((Var is Func :- Body), (Head :- PBody)) :- 
    !,
    reconstruct_prolog_head(Var, Func, Head),
    starlog_body_to_pl(Body, PBody).
starlog_to_pl((Head :- Body), (Head :- PBody)) :- 
    !, 
    starlog_body_to_pl(Body, PBody).
starlog_to_pl(Fact, Fact).

% Reconstruct a Prolog predicate head from Starlog format
reconstruct_prolog_head(OutputVar, Func, Head) :-
    Func =.. [Pred|Args],
    get_output_position(Pred, OutPos),
    insert_at_position(Args, OutPos, OutputVar, AllArgs),
    Head =.. [Pred|AllArgs].

% Get the output position for known predicates
get_output_position(string_concat, 3).
get_output_position(atom_concat, 3).
get_output_position(append, 3).
get_output_position(length, 2).
get_output_position(_, last).  % Default: add at the end

% Insert a value at a specific position in a list
insert_at_position(Args, last, Value, AllArgs) :-
    !,
    append(Args, [Value], AllArgs).
insert_at_position(Args, Pos, Value, AllArgs) :-
    length(Prefix, Pos),
    append(Prefix, Suffix, AllArgs),
    append(Prefix, [Value|Suffix], AllArgs).

% Convert Starlog body to Prolog body
starlog_body_to_pl(Body, PrologBody) :-
    flatten_conjunction(Body, Goals),
    maplist(starlog_goal_to_pl, Goals, PlGoals),
    rebuild_conjunction(PlGoals, PrologBody).

% Flatten a conjunction into a list of goals
flatten_conjunction((A,B), Goals) :- 
    !,
    flatten_conjunction(A, GoalsA),
    flatten_conjunction(B, GoalsB),
    append(GoalsA, GoalsB, Goals).
flatten_conjunction(Goal, [Goal]).

% Rebuild a conjunction from a list of goals
rebuild_conjunction([Goal], Goal) :- !.
rebuild_conjunction([Goal|Goals], (Goal,Rest)) :-
    rebuild_conjunction(Goals, Rest).

% Convert individual Starlog goals to Prolog
starlog_goal_to_pl((A,B), (PA,PB)) :- !, starlog_goal_to_pl(A, PA), starlog_goal_to_pl(B, PB).

% String and atom operations
starlog_goal_to_pl(X is (Y : Z), string_concat(Y, Z, X)) :- !.
starlog_goal_to_pl(X is (Y & Z), append(Y, Z, X)) :- !.
starlog_goal_to_pl(X is (Y • Z), atom_concat(Y, Z, X)) :- !.
starlog_goal_to_pl(X is string_length(Y), string_length(Y, X)) :- !.
starlog_goal_to_pl(X is number_string(Y), number_string(Y, X)) :- !.
starlog_goal_to_pl(X is atom_length(Y), atom_length(Y, X)) :- !.
starlog_goal_to_pl(X is sub_string(A,B,C,D), sub_string(A,B,C,D,X)) :- !.
starlog_goal_to_pl(X is string_chars(Y), string_chars(Y, X)) :- !.
starlog_goal_to_pl(X is atom_chars(Y), atom_chars(Y, X)) :- !.
starlog_goal_to_pl(X is atom_string(Y), atom_string(Y, X)) :- !.
starlog_goal_to_pl(X is atom_number(Y), atom_number(Y, X)) :- !.
starlog_goal_to_pl(X is char_code(Y), char_code(Y, X)) :- !.
starlog_goal_to_pl(X is string_upper(Y), string_upper(Y, X)) :- !.
starlog_goal_to_pl(X is string_lower(Y), string_lower(Y, X)) :- !.
starlog_goal_to_pl(X is atom_codes(Y), atom_codes(Y, X)) :- !.
starlog_goal_to_pl(X is string_codes(Y), string_codes(Y, X)) :- !.
starlog_goal_to_pl(X is term_string(Y), term_string(Y, X)) :- !.
starlog_goal_to_pl(X is term_to_atom(Y), term_to_atom(Y, X)) :- !.
starlog_goal_to_pl(X is downcase_atom(Y), downcase_atom(Y, X)) :- !.
starlog_goal_to_pl(X is upcase_atom(Y), upcase_atom(Y, X)) :- !.

% List operations
starlog_goal_to_pl(X is length_1(Y), length(Y, X)) :- !.
starlog_goal_to_pl(X is member_1(Y), member(Y, X)) :- !.
starlog_goal_to_pl(X is reverse(Y), reverse(Y, X)) :- !.
starlog_goal_to_pl(X is head(Y), head(Y, X)) :- !.
starlog_goal_to_pl(X is tail(Y), tail(Y, X)) :- !.
starlog_goal_to_pl(X is delete(Y,Z), delete(Y,Z,X)) :- !.
starlog_goal_to_pl(X is wrap(Y), wrap(Y, X)) :- !.
starlog_goal_to_pl(X is unwrap(Y), unwrap(Y, X)) :- !.
starlog_goal_to_pl(X is maplist(Y,Z), maplist(Y,Z,X)) :- !.
starlog_goal_to_pl(X is sort(Y), sort(Y, X)) :- !.
starlog_goal_to_pl(X is msort(Y), msort(Y, X)) :- !.
starlog_goal_to_pl(X is keysort(Y), keysort(Y, X)) :- !.
starlog_goal_to_pl(X is intersection(Y,Z), intersection(Y,Z,X)) :- !.
starlog_goal_to_pl(X is union(Y,Z), union(Y,Z,X)) :- !.
starlog_goal_to_pl(X is flatten(Y), flatten(Y, X)) :- !.
starlog_goal_to_pl(X is nth0(Y,Z), nth0(Y,Z,X)) :- !.
starlog_goal_to_pl(X is nth1(Y,Z), nth1(Y,Z,X)) :- !.
starlog_goal_to_pl(X is last(Y), last(Y, X)) :- !.
starlog_goal_to_pl(X is min_list(Y), min_list(Y, X)) :- !.
starlog_goal_to_pl(X is max_list(Y), max_list(Y, X)) :- !.
starlog_goal_to_pl(X is sum_list(Y), sum_list(Y, X)) :- !.
starlog_goal_to_pl(X is subtract(Y,Z), subtract(Y,Z,X)) :- !.
starlog_goal_to_pl(X is select(Y,Z), select(Y,Z,X)) :- !.
starlog_goal_to_pl(X is permutation(Y), permutation(Y, X)) :- !.

% Math operations
starlog_goal_to_pl(X is Y, is(X, Y)) :- compound(Y), \+ is_simple_starlog_operation(Y), !.
starlog_goal_to_pl(X = Y, =(X, Y)) :- !.
starlog_goal_to_pl(X is string_to_number(Y), string_to_number(Y, X)) :- !.
starlog_goal_to_pl(X is random(Y), random(Y, X)) :- !.
starlog_goal_to_pl(X is ceiling(Y), ceiling(Y, X)) :- !.
starlog_goal_to_pl(X is sqrt(Y), sqrt(Y, X)) :- !.
starlog_goal_to_pl(X is round(Y), round(Y, X)) :- !.
starlog_goal_to_pl(X is floor(Y), floor(Y, X)) :- !.
starlog_goal_to_pl(X is truncate(Y), truncate(Y, X)) :- !.
starlog_goal_to_pl(X is abs(Y), abs(Y, X)) :- !.
starlog_goal_to_pl(X is sign(Y), sign(Y, X)) :- !.
starlog_goal_to_pl(X is sin(Y), sin(Y, X)) :- !.
starlog_goal_to_pl(X is cos(Y), cos(Y, X)) :- !.
starlog_goal_to_pl(X is tan(Y), tan(Y, X)) :- !.
starlog_goal_to_pl(X is asin(Y), asin(Y, X)) :- !.
starlog_goal_to_pl(X is acos(Y), acos(Y, X)) :- !.
starlog_goal_to_pl(X is atan(Y), atan(Y, X)) :- !.
starlog_goal_to_pl(X is log(Y), log(Y, X)) :- !.
starlog_goal_to_pl(X is log10(Y), log10(Y, X)) :- !.
starlog_goal_to_pl(X is exp(Y), exp(Y, X)) :- !.

% Other operations
starlog_goal_to_pl(X is date, date(X)) :- !.
starlog_goal_to_pl(X is findall(Y,Z), findall(Y,Z,X)) :- !.
starlog_goal_to_pl(X is string_from_file(Y), string_from_file(Y, X)) :- !.
starlog_goal_to_pl(X is read_string(A,B,C,D), read_string(A,B,C,D,X)) :- !.
starlog_goal_to_pl(X is term_variables(Y), term_variables(Y, X)) :- !.
starlog_goal_to_pl(X is current_output(Y), current_output(Y, X)) :- !.
starlog_goal_to_pl(X is current_input(Y), current_input(Y, X)) :- !.
starlog_goal_to_pl(X is file_base_name(Y), file_base_name(Y, X)) :- !.
starlog_goal_to_pl(X is file_name_extension(Y,Z), file_name_extension(Y,Z,X)) :- !.
starlog_goal_to_pl(X is directory_files(Y), directory_files(Y, X)) :- !.
starlog_goal_to_pl(X is working_directory(Y), working_directory(Y, X)) :- !.
starlog_goal_to_pl(X is atomic_list_concat(Y), atomic_list_concat(Y, X)) :- !.
starlog_goal_to_pl(X is atomic_list_concat(Y,Z), atomic_list_concat(Y,Z,X)) :- !.
starlog_goal_to_pl(X is sub_atom(A,B,C,D), sub_atom(A,B,C,D,X)) :- !.
starlog_goal_to_pl(X is format_time(A,B,C), format_time(A,B,C,X)) :- !.
starlog_goal_to_pl(X is split_string(A,B,C), split_string(A,B,C,X)) :- !.
starlog_goal_to_pl(X is get_time, get_time(X)) :- !.
starlog_goal_to_pl(X is term_hash(Y), term_hash(Y, X)) :- !.

starlog_goal_to_pl(is_space(X), is_space(X)) :- !. % Keep is_space predicate as is
starlog_goal_to_pl(true, true) :- !.
starlog_goal_to_pl(Goal, Goal).

% Check if an operation is simple enough to not need special handling
is_simple_starlog_operation(X) :- var(X), !.
is_simple_starlog_operation(X) :- atomic(X), !.
is_simple_starlog_operation(_).

write_clauses_to_file(File, Clauses) :-
    setup_call_cleanup(
        open(File, append, Stream),  % Use append mode since we already wrote the header
        (write_clauses(Stream, Clauses), flush_output(Stream)),
        close(Stream)
    ).

write_clauses(_, []).
write_clauses(Stream, [Clause|Clauses]) :-
    ( Clause = (Head :- true) ->
        write_term(Stream, Head, [quoted(false), numbervars(true)]), write(Stream, '.\n')
    ; Clause = (Head :- Body) ->
        write_term(Stream, (Head :- Body), [quoted(false), numbervars(true)]), write(Stream, '.\n')
    ;   write_term(Stream, Clause, [quoted(false), numbervars(true)]), write(Stream, '.\n')
    ),
    write_clauses(Stream, Clauses).
