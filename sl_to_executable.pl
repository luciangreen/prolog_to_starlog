% Compile .sl files to standalone executables
% Author: luciangreenPlease  
% Date: 2025-07-08

:- use_module(var_utils).

% Define operators for Starlog syntax - order matters!
:- op(700, xfx, is).
:- op(600, xfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').

main :-
    create_executables_from_sl,
    halt.

create_executables_from_sl :-
    working_directory(CWD, CWD),
    format('~n=== Starlog (.sl) to Executable Compiler ===~n'),
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
    format('~n=== Executable Creation Complete ===~n').

hidden_or_special(File) :- sub_atom(File, 0, 1, _, '.').
hidden_or_special('..').
hidden_or_special('.').

is_sl_file(File) :-
    file_name_extension(_, 'sl', File).

process_sl_files([]).
process_sl_files([File|Files]) :-
    format('Creating executable for: ~w~n', [File]),
    catch(
        (create_executable(File), format('Successfully created executable for ~w~n', [File])),
        Error,
        format('Error creating executable for ~w: ~w~n', [File, Error])
    ),
    process_sl_files(Files).

create_executable(SlFile) :-
    format('Converting ~w to executable~n', [SlFile]),
    
    % Create executable shell script
    file_name_extension(Base, 'sl', SlFile),
    atom_concat(Base, '.sh', ExecFile),
    
    setup_call_cleanup(
        open(ExecFile, write, Stream),
        (
            write(Stream, '#!/bin/bash'), nl(Stream),
            write(Stream, '# Starlog executable generated from '), 
            write(Stream, SlFile), nl(Stream),
            write(Stream, '# Author: luciangreenPlease'), nl(Stream),
            write(Stream, '# Date: 2025-07-08'), nl(Stream), nl(Stream),
            write(Stream, '# Convert .sl to temporary .pl file'), nl(Stream),
            write(Stream, 'TEMP_FILE="${BASH_SOURCE%/*}/'), 
            write(Stream, Base), write(Stream, '_temp.pl"'), nl(Stream), nl(Stream),
            write(Stream, '# Convert and run'), nl(Stream),
            write(Stream, 'swipl -g "'), nl(Stream),
            write(Stream, '    consult(''starlog_to_prolog.pl''),'), nl(Stream),
            write(Stream, '    read_file_to_clauses('''), write(Stream, SlFile), 
            write(Stream, ''', Clauses0),'), nl(Stream),
            write(Stream, '    include(is_clause, Clauses0, Clauses),'), nl(Stream),
            write(Stream, '    maplist(starlog_to_pl, Clauses, PrologClauses),'), nl(Stream),
            write(Stream, '    copy_file_header('''), write(Stream, SlFile), 
            write(Stream, ''', ''$TEMP_FILE''),'), nl(Stream),
            write(Stream, '    write_clauses_to_file(''$TEMP_FILE'', PrologClauses),'), nl(Stream),
            write(Stream, '    consult(''$TEMP_FILE''),'), nl(Stream),
            write(Stream, '    delete_file(''$TEMP_FILE'')'), nl(Stream),
            write(Stream, '" -t halt'), nl(Stream),
            flush_output(Stream)
        ),
        close(Stream)
    ),
    
    % Make script executable
    atom_concat('chmod +x ', ExecFile, ChmodCmd),
    shell(ChmodCmd),
    format('Created executable: ~w~n', [ExecFile]).