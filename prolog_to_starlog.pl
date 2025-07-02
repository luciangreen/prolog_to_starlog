:- use_module(var_utils).

% Define operators for Starlog syntax - order matters!
:- op(700, xfx, is).
:- op(600, xfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').

main :-
    convert_all_prolog_to_starlog,
    halt.

convert_all_prolog_to_starlog :-
    working_directory(CWD, CWD),
    format('~n=== Prolog to Starlog Converter ===~n'),
    format('Date/Time: 2025-07-02 16:23:54 UTC~n'),
    format('User: luciangreenPlease~n'),
    format('Working in directory: ~w~n', [CWD]),
    
    % Look for files in the current directory
    directory_files('.', AllFiles),
    exclude(hidden_or_special, AllFiles, Files),
    include(is_prolog_file, Files, PrologFiles),
    ( PrologFiles = [] ->
        format('No .pl files found for conversion~n')
    ; length(PrologFiles, NumFiles),
      format('Found ~w files to process: ~w~n', [NumFiles, PrologFiles]),
      process_prolog_files(PrologFiles)
    ),
    format('~n=== Prolog to Starlog Conversion Complete ===~n').

hidden_or_special(File) :- sub_atom(File, 0, 1, _, '.').
hidden_or_special('..').
hidden_or_special('.').

is_prolog_file(File) :-
    file_name_extension(_, 'pl', File),
    \+ sub_atom(File, _, _, 0, '_starlog'),
    \+ sub_atom(File, _, _, 0, '_prolog'),
    File \= 'var_utils.pl',
    File \= 'prolog_to_starlog.pl',
    File \= 'starlog_to_prolog.pl'.

process_prolog_files([]).
process_prolog_files([File|Files]) :-
    format('Processing file: ~w~n', [File]),
    catch(
        (process_single_file(File), format('Successfully processed ~w~n', [File])),
        Error,
        format('Error processing ~w: ~w~n', [File, Error])
    ),
    process_prolog_files(Files).

process_single_file(InputFile) :-
    format('Reading file: ~w~n', [InputFile]),
    read_file_to_clauses(InputFile, Clauses0),
    length(Clauses0, NumRaw),
    format('Read ~w raw clauses from ~w~n', [NumRaw, InputFile]),
    include(is_clause, Clauses0, Clauses),
    length(Clauses, NumFiltered),
    format('Filtered to ~w valid clauses~n', [NumFiltered]),
    maplist(pl_to_starlog, Clauses, StarlogClauses),
    length(StarlogClauses, NumConverted),
    format('Converted ~w clauses to Starlog~n', [NumConverted]),
    maplist(rename_vars_pretty, StarlogClauses, PrettyClauses),
    
    % Extract base filename and create output path
    file_name_extension(Base, 'pl', InputFile),
    atom_concat(Base, '_starlog.pl', OutputFile),
    
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

% --- Prolog clause to Starlog clause converter ---
pl_to_starlog((Head :- Body), (StarHead :- SBody)) :- 
    !, 
    extract_output_var(Head, OutputVar, StarHead),
    pl_body_to_starlog(Body, OutputVar, SBody).
pl_to_starlog(Fact, StarFact) :-
    extract_output_var(Fact, _, StarFact).

% Extract the output variable from a predicate head
extract_output_var(Head, OutputVar, StarHead) :-
    Head =.. [Pred|Args],
    (   get_predicate_output_position(Pred, Args, OutPos),
        OutPos > 0
    ->  nth1(OutPos, Args, OutputVar),
        delete_nth(Args, OutPos, NewArgs),
        StarHead =.. [Pred|NewArgs]
    ;   StarHead = Head,
        OutputVar = none
    ).

% Find the output position for known predicates
% String/Atom Operations
get_predicate_output_position(string_concat, [_,_,Out], 3) :- !.
get_predicate_output_position(atom_concat, [_,_,Out], 3) :- !.
get_predicate_output_position(string_length, [_,Out], 2) :- !.
get_predicate_output_position(number_string, [_,Out], 2) :- !.
get_predicate_output_position(atom_length, [_,Out], 2) :- !.
get_predicate_output_position(sub_string, [_,_,_,_,Out], 5) :- !.
get_predicate_output_position(string_chars, [_,Out], 2) :- !.
get_predicate_output_position(atom_chars, [_,Out], 2) :- !.
get_predicate_output_position(atom_string, [_,Out], 2) :- !.
get_predicate_output_position(atom_number, [_,Out], 2) :- !.
get_predicate_output_position(char_code, [_,Out], 2) :- !.
get_predicate_output_position(string_upper, [_,Out], 2) :- !.
get_predicate_output_position(string_lower, [_,Out], 2) :- !.
get_predicate_output_position(atom_codes, [_,Out], 2) :- !.
get_predicate_output_position(string_codes, [_,Out], 2) :- !.
get_predicate_output_position(term_string, [_,Out], 2) :- !.
get_predicate_output_position(term_to_atom, [_,Out], 2) :- !.
get_predicate_output_position(downcase_atom, [_,Out], 2) :- !.
get_predicate_output_position(upcase_atom, [_,Out], 2) :- !.

% List Operations
get_predicate_output_position(append, [_,_,Out], 3) :- !.
get_predicate_output_position(length, [_,Out], 2) :- !.
get_predicate_output_position(member, [_,Out], 2) :- !.
get_predicate_output_position(reverse, [_,Out], 2) :- !.
get_predicate_output_position(head, [_,Out], 2) :- !.
get_predicate_output_position(tail, [_,Out], 2) :- !.
get_predicate_output_position(delete, [_,_,Out], 3) :- !.
get_predicate_output_position(wrap, [_,Out], 2) :- !.
get_predicate_output_position(unwrap, [_,Out], 2) :- !.
get_predicate_output_position(maplist, [_,_,Out], 3) :- !.
get_predicate_output_position(sort, [_,Out], 2) :- !.
get_predicate_output_position(msort, [_,Out], 2) :- !.
get_predicate_output_position(keysort, [_,Out], 2) :- !.
get_predicate_output_position(intersection, [_,_,Out], 3) :- !.
get_predicate_output_position(union, [_,_,Out], 3) :- !.
get_predicate_output_position(flatten, [_,Out], 2) :- !.
get_predicate_output_position(nth0, [_,_,Out], 3) :- !.
get_predicate_output_position(nth1, [_,_,Out], 3) :- !.
get_predicate_output_position(last, [_,Out], 2) :- !.
get_predicate_output_position(min_list, [_,Out], 2) :- !.
get_predicate_output_position(max_list, [_,Out], 2) :- !.
get_predicate_output_position(sum_list, [_,Out], 2) :- !.
get_predicate_output_position(subtract, [_,_,Out], 3) :- !.
get_predicate_output_position(select, [_,_,Out], 3) :- !.
get_predicate_output_position(permutation, [_,Out], 2) :- !.

% Math Operations
get_predicate_output_position(string_to_number, [_,Out], 2) :- !.
get_predicate_output_position(random, [_,Out], 2) :- !.
get_predicate_output_position(ceiling, [_,Out], 2) :- !.
get_predicate_output_position(sqrt, [_,Out], 2) :- !.
get_predicate_output_position(round, [_,Out], 2) :- !.
get_predicate_output_position(floor, [_,Out], 2) :- !.
get_predicate_output_position(truncate, [_,Out], 2) :- !.
get_predicate_output_position(abs, [_,Out], 2) :- !.
get_predicate_output_position(sign, [_,Out], 2) :- !.
get_predicate_output_position(sin, [_,Out], 2) :- !.
get_predicate_output_position(cos, [_,Out], 2) :- !.
get_predicate_output_position(tan, [_,Out], 2) :- !.
get_predicate_output_position(asin, [_,Out], 2) :- !.
get_predicate_output_position(acos, [_,Out], 2) :- !.
get_predicate_output_position(atan, [_,Out], 2) :- !.
get_predicate_output_position(log, [_,Out], 2) :- !.
get_predicate_output_position(log10, [_,Out], 2) :- !.
get_predicate_output_position(exp, [_,Out], 2) :- !.

% Other Operations
get_predicate_output_position(date, [Out], 1) :- !.
get_predicate_output_position(findall, [_,_,Out], 3) :- !.
get_predicate_output_position(string_from_file, [_,Out], 2) :- !.
get_predicate_output_position(read_string, [_,_,_,_,Out], 5) :- !.
get_predicate_output_position(term_variables, [_,Out], 2) :- !.
get_predicate_output_position(current_output, [_,Out], 2) :- !.
get_predicate_output_position(current_input, [_,Out], 2) :- !.
get_predicate_output_position(file_base_name, [_,Out], 2) :- !.
get_predicate_output_position(file_name_extension, [_,_,Out], 3) :- !.
get_predicate_output_position(directory_files, [_,Out], 2) :- !.
get_predicate_output_position(working_directory, [_,Out], 2) :- !.
get_predicate_output_position(atomic_list_concat, [_,Out], 2) :- !.
get_predicate_output_position(atomic_list_concat, [_,_,Out], 3) :- !.
get_predicate_output_position(sub_atom, [_,_,_,_,Out], 5) :- !.
get_predicate_output_position(format_time, [_,_,_,Out], 4) :- !.
get_predicate_output_position(split_string, [_,_,_,Out], 4) :- !.
get_predicate_output_position(get_time, [Out], 1) :- !.
get_predicate_output_position(term_hash, [_,Out], 2) :- !.

% Default - no output position
get_predicate_output_position(_, _, 0).

% Delete the nth element from a list
delete_nth(List, N, Result) :-
    length(Prefix, N),
    append(Prefix, [_|Suffix], List),
    append(Prefix, Suffix, Result).

% Convert Prolog body to Starlog body
pl_body_to_starlog(Body, _OutputVar, StarlogBody) :-
    flatten_conjunction(Body, Goals),
    maplist(pl_goal_to_starlog, Goals, StarlogGoals),
    rebuild_conjunction(StarlogGoals, StarlogBody).

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

% Convert individual Prolog goals to Starlog
pl_goal_to_starlog((A,B), (SA,SB)) :- !, pl_goal_to_starlog(A, SA), pl_goal_to_starlog(B, SB).

% String and Atom operations
pl_goal_to_starlog(string_concat(A,B,C), (C is (A : B))) :- !.
pl_goal_to_starlog(atom_concat(A,B,C), (C is (A • B))) :- !.
pl_goal_to_starlog(string_length(A,B), (B is string_length(A))) :- !.
pl_goal_to_starlog(number_string(A,B), (B is number_string(A))) :- !.
pl_goal_to_starlog(atom_length(A,B), (B is atom_length(A))) :- !.
pl_goal_to_starlog(sub_string(A,B,C,D,E), (E is sub_string(A,B,C,D))) :- !.
pl_goal_to_starlog(string_chars(A,B), (B is string_chars(A))) :- !.
pl_goal_to_starlog(atom_chars(A,B), (B is atom_chars(A))) :- !.
pl_goal_to_starlog(atom_string(A,B), (B is atom_string(A))) :- !.
pl_goal_to_starlog(atom_number(A,B), (B is atom_number(A))) :- !.
pl_goal_to_starlog(char_code(A,B), (B is char_code(A))) :- !.
pl_goal_to_starlog(string_upper(A,B), (B is string_upper(A))) :- !.
pl_goal_to_starlog(string_lower(A,B), (B is string_lower(A))) :- !.
pl_goal_to_starlog(atom_codes(A,B), (B is atom_codes(A))) :- !.
pl_goal_to_starlog(string_codes(A,B), (B is string_codes(A))) :- !.
pl_goal_to_starlog(term_string(A,B), (B is term_string(A))) :- !.
pl_goal_to_starlog(term_to_atom(A,B), (B is term_to_atom(A))) :- !.
pl_goal_to_starlog(downcase_atom(A,B), (B is downcase_atom(A))) :- !.
pl_goal_to_starlog(upcase_atom(A,B), (B is upcase_atom(A))) :- !.

% List operations
pl_goal_to_starlog(append(A,B,C), (C is (A & B))) :- !.
pl_goal_to_starlog(length(A,B), (B is length_1(A))) :- !.
pl_goal_to_starlog(member(A,B), (B is member_1(A))) :- !.
pl_goal_to_starlog(reverse(A,B), (B is reverse(A))) :- !.
pl_goal_to_starlog(head(A,B), (B is head(A))) :- !.
pl_goal_to_starlog(tail(A,B), (B is tail(A))) :- !.
pl_goal_to_starlog(delete(A,B,C), (C is delete(A,B))) :- !.
pl_goal_to_starlog(wrap(A,B), (B is wrap(A))) :- !.
pl_goal_to_starlog(unwrap(A,B), (B is unwrap(A))) :- !.
pl_goal_to_starlog(maplist(A,B,C), (C is maplist(A,B))) :- !.
pl_goal_to_starlog(sort(A,B), (B is sort(A))) :- !.
pl_goal_to_starlog(msort(A,B), (B is msort(A))) :- !.
pl_goal_to_starlog(keysort(A,B), (B is keysort(A))) :- !.
pl_goal_to_starlog(intersection(A,B,C), (C is intersection(A,B))) :- !.
pl_goal_to_starlog(union(A,B,C), (C is union(A,B))) :- !.
pl_goal_to_starlog(flatten(A,B), (B is flatten(A))) :- !.
pl_goal_to_starlog(nth0(A,B,C), (C is nth0(A,B))) :- !.
pl_goal_to_starlog(nth1(A,B,C), (C is nth1(A,B))) :- !.
pl_goal_to_starlog(last(A,B), (B is last(A))) :- !.
pl_goal_to_starlog(min_list(A,B), (B is min_list(A))) :- !.
pl_goal_to_starlog(max_list(A,B), (B is max_list(A))) :- !.
pl_goal_to_starlog(sum_list(A,B), (B is sum_list(A))) :- !.
pl_goal_to_starlog(subtract(A,B,C), (C is subtract(A,B))) :- !.
pl_goal_to_starlog(select(A,B,C), (C is select(A,B))) :- !.
pl_goal_to_starlog(permutation(A,B), (B is permutation(A))) :- !.

% Math operations
pl_goal_to_starlog(is(A,B), (A is B)) :- !.
pl_goal_to_starlog(=(A,B), (A = B)) :- !.
pl_goal_to_starlog(string_to_number(A,B), (B is string_to_number(A))) :- !.
pl_goal_to_starlog(random(A,B), (B is random(A))) :- !.
pl_goal_to_starlog(ceiling(A,B), (B is ceiling(A))) :- !.
pl_goal_to_starlog(sqrt(A,B), (B is sqrt(A))) :- !.
pl_goal_to_starlog(round(A,B), (B is round(A))) :- !.
pl_goal_to_starlog(floor(A,B), (B is floor(A))) :- !.
pl_goal_to_starlog(truncate(A,B), (B is truncate(A))) :- !.
pl_goal_to_starlog(abs(A,B), (B is abs(A))) :- !.
pl_goal_to_starlog(sign(A,B), (B is sign(A))) :- !.
pl_goal_to_starlog(sin(A,B), (B is sin(A))) :- !.
pl_goal_to_starlog(cos(A,B), (B is cos(A))) :- !.
pl_goal_to_starlog(tan(A,B), (B is tan(A))) :- !.
pl_goal_to_starlog(asin(A,B), (B is asin(A))) :- !.
pl_goal_to_starlog(acos(A,B), (B is acos(A))) :- !.
pl_goal_to_starlog(atan(A,B), (B is atan(A))) :- !.
pl_goal_to_starlog(log(A,B), (B is log(A))) :- !.
pl_goal_to_starlog(log10(A,B), (B is log10(A))) :- !.
pl_goal_to_starlog(exp(A,B), (B is exp(A))) :- !.

% Other operations
pl_goal_to_starlog(date(A), (A is date)) :- !.
pl_goal_to_starlog(findall(A,B,C), (C is findall(A,B))) :- !.
pl_goal_to_starlog(string_from_file(A,B), (B is string_from_file(A))) :- !.
pl_goal_to_starlog(read_string(A,B,C,D,E), (E is read_string(A,B,C,D))) :- !.
pl_goal_to_starlog(term_variables(A,B), (B is term_variables(A))) :- !.
pl_goal_to_starlog(current_output(A,B), (B is current_output(A))) :- !.
pl_goal_to_starlog(current_input(A,B), (B is current_input(A))) :- !.
pl_goal_to_starlog(file_base_name(A,B), (B is file_base_name(A))) :- !.
pl_goal_to_starlog(file_name_extension(A,B,C), (C is file_name_extension(A,B))) :- !.
pl_goal_to_starlog(directory_files(A,B), (B is directory_files(A))) :- !.
pl_goal_to_starlog(working_directory(A,B), (B is working_directory(A))) :- !.
pl_goal_to_starlog(atomic_list_concat(A,B), (B is atomic_list_concat(A))) :- !.
pl_goal_to_starlog(atomic_list_concat(A,B,C), (C is atomic_list_concat(A,B))) :- !.
pl_goal_to_starlog(sub_atom(A,B,C,D,E), (E is sub_atom(A,B,C,D))) :- !.
pl_goal_to_starlog(format_time(A,B,C,D), (D is format_time(A,B,C))) :- !.
pl_goal_to_starlog(split_string(A,B,C,D), (D is split_string(A,B,C))) :- !.
pl_goal_to_starlog(get_time(A), (A is get_time)) :- !.
pl_goal_to_starlog(term_hash(A,B), (B is term_hash(A))) :- !.

pl_goal_to_starlog(is_space(X), is_space(X)) :- !. % Keep is_space predicate as is
pl_goal_to_starlog(true, true) :- !.
pl_goal_to_starlog(Goal, Goal).

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
