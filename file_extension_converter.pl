% File Extension Converter for Prolog to Starlog project
% Author: luciangreenPlease  
% Date: 2025-07-08 23:20:46 UTC
% Converts between .pl and .sl file formats

:- use_module(library(filesex)).

main :-
    convert_file_extensions,
    halt.

convert_file_extensions :-
    working_directory(CWD, CWD),
    format('~n=== File Extension Converter ===~n'),
    format('Date/Time: 2025-07-08 23:20:46 UTC~n'),
    format('User: luciangreenPlease~n'),
    format('Working in directory: ~w~n', [CWD]),
    
    % Look for both .pl and .sl files
    directory_files('.', AllFiles),
    exclude(hidden_or_special, AllFiles, Files),
    include(is_convertible_file, Files, ConvertibleFiles),
    
    ( ConvertibleFiles = [] ->
        format('No .pl or .sl files found for conversion~n')
    ; length(ConvertibleFiles, NumFiles),
      format('Found ~w files to process: ~w~n', [NumFiles, ConvertibleFiles]),
      process_files(ConvertibleFiles)
    ),
    format('~n=== File Extension Conversion Complete ===~n').

hidden_or_special(File) :- sub_atom(File, 0, 1, _, '.').
hidden_or_special('..').
hidden_or_special('.').

is_convertible_file(File) :-
    (file_name_extension(_, 'pl', File) ; file_name_extension(_, 'sl', File)),
    File \= 'var_utils.pl',
    File \= 'prolog_to_starlog.pl',
    File \= 'starlog_to_prolog.pl',
    File \= 'file_extension_converter.pl',
    \+ sub_atom(File, _, _, 0, '_starlog'),
    \+ sub_atom(File, _, _, 0, '_prolog').

process_files([]).
process_files([File|Files]) :-
    format('Processing file: ~w~n', [File]),
    catch(
        (convert_file_extension(File), format('Successfully converted ~w~n', [File])),
        Error,
        format('Error converting ~w: ~w~n', [File, Error])
    ),
    process_files(Files).

convert_file_extension(InputFile) :-
    file_name_extension(Base, Ext, InputFile),
    ( Ext = 'pl' ->
        atom_concat(Base, '.sl', OutputFile),
        format('Converting ~w to ~w~n', [InputFile, OutputFile])
    ; Ext = 'sl' ->
        atom_concat(Base, '.pl', OutputFile),
        format('Converting ~w to ~w~n', [InputFile, OutputFile])
    ; format('Unknown extension for file: ~w~n', [InputFile]),
      fail
    ),
    
    % Copy the file with the new extension
    copy_file(InputFile, OutputFile),
    format('Created: ~w~n', [OutputFile]).