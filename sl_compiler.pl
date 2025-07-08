% Compile and run .sl files
% Author: luciangreenPlease
% Date: 2025-07-08

:- use_module(var_utils).

% Define operators for Starlog syntax - order matters!
:- op(700, xfx, is).
:- op(600, xfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').

main :-
    compile_and_run_sl_files,
    halt.

compile_and_run_sl_files :-
    working_directory(CWD, CWD),
    format('~n=== Starlog (.sl) Compiler and Runner ===~n'),
    format('Date/Time: 2025-07-08 16:23:54 UTC~n'),
    format('User: luciangreenPlease~n'),
    format('Working in directory: ~w~n', [CWD]),
    
    % Look for .sl files in the current directory
    directory_files('.', AllFiles),
    exclude(hidden_or_special, AllFiles, Files),
    include(is_sl_file, Files, SlFiles),
    ( SlFiles = [] ->
        format('No .sl files found to compile~n')
    ; length(SlFiles, NumFiles),
      format('Found ~w .sl files to compile: ~w~n', [NumFiles, SlFiles]),
      process_sl_files(SlFiles)
    ),
    format('~n=== Starlog Compilation Complete ===~n').

hidden_or_special(File) :- sub_atom(File, 0, 1, _, '.').
hidden_or_special('..').
hidden_or_special('.').

is_sl_file(File) :-
    file_name_extension(_, 'sl', File).

process_sl_files([]).
process_sl_files([File|Files]) :-
    format('Compiling file: ~w~n', [File]),
    catch(
        (compile_and_run_sl_file(File), format('Successfully compiled and ran ~w~n', [File])),
        Error,
        format('Error compiling ~w: ~w~n', [File, Error])
    ),
    process_sl_files(Files).

compile_and_run_sl_file(SlFile) :-
    format('Converting ~w to Prolog for compilation~n', [SlFile]),
    
    % Convert .sl to .pl first
    file_name_extension(Base, 'sl', SlFile),
    atom_concat(Base, '_temp.pl', TempFile),
    
    % Use starlog_to_prolog to convert the file
    consult('starlog_to_prolog.pl'),
    
    % Read and convert the .sl file
    read_file_to_clauses(SlFile, Clauses0),
    length(Clauses0, NumRaw),
    format('Read ~w raw clauses from ~w~n', [NumRaw, SlFile]),
    include(is_clause, Clauses0, Clauses),
    length(Clauses, NumFiltered),
    format('Filtered to ~w valid clauses~n', [NumFiltered]),
    
    % Convert using starlog_to_prolog predicates
    maplist(starlog_to_pl, Clauses, PrologClauses),
    length(PrologClauses, NumConverted),
    format('Converted ~w clauses to Prolog~n', [NumConverted]),
    
    % Write temporary .pl file
    copy_file_header(SlFile, TempFile),
    write_clauses_to_file(TempFile, PrologClauses),
    
    % Load and run the converted Prolog file
    format('Loading and running: ~w~n', [TempFile]),
    consult(TempFile),
    
    % Clean up temporary file
    delete_file(TempFile),
    format('Cleaned up temporary file: ~w~n', [TempFile]).

% Include necessary predicates
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
        read_all_clauses(Stream, Clauses),
        close(Stream)
    ).

read_all_clauses(Stream, [Clause|Clauses]) :-
    read_term(Stream, Clause, []),
    Clause \= end_of_file, !,
    read_all_clauses(Stream, Clauses).
read_all_clauses(_, []).

write_clauses_to_file(File, Clauses) :-
    setup_call_cleanup(
        open(File, append, Stream),
        (write_clauses(Stream, Clauses), flush_output(Stream)),
        close(Stream)
    ).

write_clauses(_, []).
write_clauses(Stream, [Clause|Clauses]) :-
    ( Clause = (Head :- true) ->
        format(Stream, '~q.~n', [Head])
    ; format(Stream, '~q.~n', [Clause])
    ),
    write_clauses(Stream, Clauses).