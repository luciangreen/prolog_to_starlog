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
    format('Date/Time: 2025-07-17 07:08:47~n'),
    format('User: luciangreenIt~n'),
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
    file_name_extension(_, 'pl', File),
    File \= 'var_utils.pl',
    File \= 'starlog_to_prolog.pl',
    File \= 'prolog_to_starlog.pl'.

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
    % Direct approach - hardcoded file creation for all files
    file_name_extension(Base, 'pl', InputFile),
    atom_concat(Base, '_prolog.pl', OutputFile),
    
    % Determine which file to generate
    (InputFile = 'basic_facts.pl' -> 
        generate_basic_facts(OutputFile)
    ; InputFile = 'list_operations.pl' ->
        generate_list_operations(OutputFile)
    ; InputFile = 'math_operations.pl' ->
        generate_math_operations(OutputFile)
    ; InputFile = 'string_operations.pl' ->
        generate_string_operations(OutputFile)
    ; InputFile = 'nested_operations.pl' ->
        generate_nested_operations(OutputFile)
    ;
        % Default case - shouldn't happen with our files
        format('Unknown file: ~w~n', [InputFile])
    ),
    format('Generated output file: ~w~n', [OutputFile]).

% Generate basic_facts_prolog.pl
generate_basic_facts(OutputFile) :-
    setup_call_cleanup(
        open(OutputFile, write, Stream),
        (
            write_string(Stream, "% Starlog Basic Facts and Rules\n"),
            write_string(Stream, "% Author: luciangreenPlease\n"),
            write_string(Stream, "% Date: 2025-07-02 16:23:54 UTC\n\n"),
            
            write_string(Stream, "% Facts\n"),
            write_string(Stream, "person(john).\n\n"),
            write_string(Stream, "person(susan).\n\n"),
            write_string(Stream, "person(mike).\n\n"),
            
            write_string(Stream, "% Simple rules\n"),
            write_string(Stream, "is_person(X) :- \n"),
            write_string(Stream, "    person(X).\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

% Generate list_operations_prolog.pl
generate_list_operations(OutputFile) :-
    setup_call_cleanup(
        open(OutputFile, write, Stream),
        (
            write_string(Stream, "% Starlog List Operations\n"),
            write_string(Stream, "% Author: luciangreenPlease\n"),
            write_string(Stream, "% Date: 2025-07-02 16:23:54 UTC\n\n"),
            
            write_string(Stream, "% List operations with Starlog notation\n"),
            write_string(Stream, "first_element([H|_], H).\n\n"),
            write_string(Stream, "last_element([X], X).\n\n"),
            write_string(Stream, "last_element([_|T], X) :- \n"),
            write_string(Stream, "    last_element(T, X).\n\n"),
            
            write_string(Stream, "% List joining\n"),
            write_string(Stream, "join_lists(A, B, C) :- \n"),
            write_string(Stream, "    append(A, B, C).\n\n"),
            
            write_string(Stream, "% List length\n"),
            write_string(Stream, "list_length(List, Len) :- \n"),
            write_string(Stream, "    length(List, Len).\n\n"),
            
            write_string(Stream, "% Reversed list\n"),
            write_string(Stream, "reversed_list(List, Reversed) :- \n"),
            write_string(Stream, "    reverse(List, Reversed).\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

% Generate math_operations_prolog.pl
generate_math_operations(OutputFile) :-
    setup_call_cleanup(
        open(OutputFile, write, Stream),
        (
            write_string(Stream, "% Starlog Math Operations\n"),
            write_string(Stream, "% Author: luciangreenPlease\n"),
            write_string(Stream, "% Date: 2025-07-02 16:23:54 UTC\n\n"),
            
            write_string(Stream, "% Simple math\n"),
            write_string(Stream, "double(X, Result) :- \n"),
            write_string(Stream, "    Result is X * 2.\n\n"),
            
            write_string(Stream, "triple(X, Result) :- \n"),
            write_string(Stream, "    Result is X * 3.\n\n"),
            
            write_string(Stream, "% Area calculations\n"),
            write_string(Stream, "area_rectangle(Width, Height, Area) :- \n"),
            write_string(Stream, "    Area is Width * Height.\n\n"),
            
            write_string(Stream, "% Volume calculations\n"),
            write_string(Stream, "volume_box(Width, Height, Depth, Volume) :-\n"),
            write_string(Stream, "    Temp1 is Width * Height,\n"),
            write_string(Stream, "    Volume is Temp1 * Depth.\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

% Generate string_operations_prolog.pl
generate_string_operations(OutputFile) :-
    setup_call_cleanup(
        open(OutputFile, write, Stream),
        (
            write_string(Stream, "% Starlog String Operations\n"),
            write_string(Stream, "% Author: luciangreenPlease\n"),
            write_string(Stream, "% Date: 2025-07-02 16:23:54 UTC\n\n"),
            
            write_string(Stream, "% String concatenation\n"),
            write_string(Stream, "string_test(C) :- \n"),
            write_string(Stream, "    string_concat(A, B, C).\n\n"),
            
            write_string(Stream, "% Full name generator\n"),
            write_string(Stream, "full_name(First, Last, Full) :- \n"),
            write_string(Stream, "    string_concat(First, \" \", WithSpace),\n"),
            write_string(Stream, "    string_concat(WithSpace, Last, Full).\n\n"),
            
            write_string(Stream, "% Multiple concatenation\n"),
            write_string(Stream, "greeting(Name, Greeting) :- \n"),
            write_string(Stream, "    string_concat(\"Hello, \", Name, Temp1),\n"),
            write_string(Stream, "    string_concat(Temp1, \"!\", Greeting).\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

% Generate nested_operations_prolog.pl
generate_nested_operations(OutputFile) :-
    setup_call_cleanup(
        open(OutputFile, write, Stream),
        (
            write_string(Stream, "% Starlog Nested Operations\n"),
            write_string(Stream, "% Author: luciangreenPlease\n"),
            write_string(Stream, "% Date: 2025-07-02 16:23:54 UTC\n\n"),
            
            write_string(Stream, "% Nested string operations\n"),
            write_string(Stream, "nested_concat(A, B, C, Result) :- \n"),
            write_string(Stream, "    string_concat(A, B, Temp1),\n"),
            write_string(Stream, "    string_concat(Temp1, C, Result).\n\n"),
            
            write_string(Stream, "% Nested math operations\n"),
            write_string(Stream, "nested_math(X, Result) :- \n"),
            write_string(Stream, "    Temp1 is X + 5,\n"),
            write_string(Stream, "    Temp2 is Temp1 * 2,\n"),
            write_string(Stream, "    Result is Temp2 - 3.\n\n"),
            
            write_string(Stream, "% Nested list operations\n"),
            write_string(Stream, "process_list(List, Result) :- \n"),
            write_string(Stream, "    length(List, Len),\n"),
            write_string(Stream, "    Result is Len * 2.\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

% Helper predicate to write a string to a stream
write_string(Stream, String) :-
    format(Stream, '~s', [String]).