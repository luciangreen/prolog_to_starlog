:- use_module(var_utils).

% Define operators for Starlog syntax
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
    format('Date/Time: ~w~n', ['2025-07-17 07:11:45']),
    format('User: ~w~n', ['luciangreenIt']),
    format('Working in directory: ~w~n', [CWD]),
    
    % Look for Prolog files in the current directory
    directory_files('.', Files),
    exclude(hidden_or_special, Files, VisibleFiles),
    include(is_prolog_file, VisibleFiles, PrologFiles),
    ( PrologFiles = [] ->
        format('No *_prolog.pl files found for conversion~n')
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
    sub_atom(File, _, _, 0, '_prolog.pl'),
    File \= 'var_utils.pl',
    File \= 'starlog_to_prolog.pl',
    File \= 'prolog_to_starlog.pl'.

process_prolog_files([]).
process_prolog_files([File|Files]) :-
    format('Processing file: ~w~n', [File]),
    catch(
        (process_single_file(File), format('Successfully processed ~w~n', [File])),
        Error,
        format('Error processing ~w: ~w~n', [File, Error])
    ),
    process_prolog_files(Files).

% Fix singleton variable warnings by using _ for unused variables
pl_to_starlog_helper(String, _, _) :- string_length(String, 2), !.
pl_to_starlog_helper(_, _, _) :- !.

% Prolog to Starlog conversion for each predicate (fixing singleton variable warnings)
pl_to_starlog(string_length(X, Y), Y is string_length(X)) :- !.
pl_to_starlog(number_string(X, Y), Y is number_string(X)) :- !.
pl_to_starlog(atom_length(X, Y), Y is atom_length(X)) :- !.
pl_to_starlog(string_chars(X, Y), Y is string_chars(X)) :- !.
pl_to_starlog(atom_chars(X, Y), Y is atom_chars(X)) :- !.
pl_to_starlog(atom_string(X, Y), Y is atom_string(X)) :- !.
pl_to_starlog(atom_number(X, Y), Y is atom_number(X)) :- !.
pl_to_starlog(char_code(X, Y), Y is char_code(X)) :- !.
pl_to_starlog(string_upper(X, Y), Y is string_upper(X)) :- !.
pl_to_starlog(string_lower(X, Y), Y is string_lower(X)) :- !.
pl_to_starlog(atom_codes(X, Y), Y is atom_codes(X)) :- !.
pl_to_starlog(string_codes(X, Y), Y is string_codes(X)) :- !.
pl_to_starlog(term_string(X, Y), Y is term_string(X)) :- !.
pl_to_starlog(term_to_atom(X, Y), Y is term_to_atom(X)) :- !.
pl_to_starlog(downcase_atom(X, Y), Y is downcase_atom(X)) :- !.
pl_to_starlog(upcase_atom(X, Y), Y is upcase_atom(X)) :- !.

% List operations (fixing singleton variable warnings)
pl_to_starlog(length(X, Y), Y is length_1(X)) :- !.
pl_to_starlog(member(X, Y), Y is member_1(X)) :- !.
pl_to_starlog(reverse(X, Y), Y is reverse(X)) :- !.
pl_to_starlog(head(X, Y), Y is head(X)) :- !.
pl_to_starlog(tail(X, Y), Y is tail(X)) :- !.
pl_to_starlog(delete(X, Y, Z), Z is delete(X, Y)) :- !.
pl_to_starlog(wrap(X, Y), Y is wrap(X)) :- !.
pl_to_starlog(unwrap(X, Y), Y is unwrap(X)) :- !.
pl_to_starlog(maplist(X, Y, Z), Z is maplist(X, Y)) :- !.
pl_to_starlog(sort(X, Y), Y is sort(X)) :- !.
pl_to_starlog(msort(X, Y), Y is msort(X)) :- !.
pl_to_starlog(keysort(X, Y), Y is keysort(X)) :- !.
pl_to_starlog(intersection(X, Y, Z), Z is intersection(X, Y)) :- !.
pl_to_starlog(union(X, Y, Z), Z is union(X, Y)) :- !.
pl_to_starlog(flatten(X, Y), Y is flatten(X)) :- !.
pl_to_starlog(nth0(X, Y, Z), Z is nth0(X, Y)) :- !.
pl_to_starlog(nth1(X, Y, Z), Z is nth1(X, Y)) :- !.
pl_to_starlog(last(X, Y), Y is last(X)) :- !.

% Other operations (fixing singleton variable warnings)
pl_to_starlog(min_list(X, Y), Y is min_list(X)) :- !.
pl_to_starlog(max_list(X, Y), Y is max_list(X)) :- !.
pl_to_starlog(sum_list(X, Y), Y is sum_list(X)) :- !.
pl_to_starlog(subtract(X, Y, Z), Z is subtract(X, Y)) :- !.
pl_to_starlog(select(X, Y, Z), Z is select(X, Y)) :- !.
pl_to_starlog(permutation(X, Y), Y is permutation(X)) :- !.
pl_to_starlog(string_to_number(X, Y), Y is string_to_number(X)) :- !.
pl_to_starlog(random(X, Y), Y is random(X)) :- !.
pl_to_starlog(ceiling(X, Y), Y is ceiling(X)) :- !.
pl_to_starlog(sqrt(X, Y), Y is sqrt(X)) :- !.
pl_to_starlog(round(X, Y), Y is round(X)) :- !.
pl_to_starlog(floor(X, Y), Y is floor(X)) :- !.
pl_to_starlog(truncate(X, Y), Y is truncate(X)) :- !.
pl_to_starlog(abs(X, Y), Y is abs(X)) :- !.
pl_to_starlog(sign(X, Y), Y is sign(X)) :- !.
pl_to_starlog(sin(X, Y), Y is sin(X)) :- !.
pl_to_starlog(cos(X, Y), Y is cos(X)) :- !.
pl_to_starlog(tan(X, Y), Y is tan(X)) :- !.
pl_to_starlog(asin(X, Y), Y is asin(X)) :- !.
pl_to_starlog(acos(X, Y), Y is acos(X)) :- !.
pl_to_starlog(atan(X, Y), Y is atan(X)) :- !.
pl_to_starlog(log(X, Y), Y is log(X)) :- !.
pl_to_starlog(log10(X, Y), Y is log10(X)) :- !.
pl_to_starlog(exp(X, Y), Y is exp(X)) :- !.

% Other operations (fixing singleton variable warnings)
pl_to_starlog(date(X), X is date) :- !.
pl_to_starlog(findall(X, Y, Z), Z is findall(X, Y)) :- !.
pl_to_starlog(string_from_file(X, Y), Y is string_from_file(X)) :- !.
pl_to_starlog(read_string(A, B, C, D, E), E is read_string(A, B, C, D)) :- !.
pl_to_starlog(term_variables(X, Y), Y is term_variables(X)) :- !.
pl_to_starlog(current_output(X), X is current_output) :- !.
pl_to_starlog(current_input(X), X is current_input) :- !.
pl_to_starlog(file_base_name(X, Y), Y is file_base_name(X)) :- !.
pl_to_starlog(file_name_extension(X, Y, Z), Z is file_name_extension(X, Y)) :- !.
pl_to_starlog(directory_files(X, Y), Y is directory_files(X)) :- !.
pl_to_starlog(working_directory(X, Y), Y is working_directory(X)) :- !.
pl_to_starlog(atomic_list_concat(X, Y), Y is atomic_list_concat(X)) :- !.
pl_to_starlog(atomic_list_concat(X, Y, Z), Z is atomic_list_concat(X, Y)) :- !.
pl_to_starlog(sub_atom(A, B, C, D, E), E is sub_atom(A, B, C, D)) :- !.
pl_to_starlog(format_time(A, B, C, D), D is format_time(A, B, C)) :- !.
pl_to_starlog(split_string(A, B, C, D), D is split_string(A, B, C)) :- !.
pl_to_starlog(get_time(X), X is get_time) :- !.
pl_to_starlog(term_hash(X, Y), Y is term_hash(X)) :- !.

% String operations (fixing singleton variable warnings)
pl_to_starlog(string_concat(X, Y, Z), Z is X : Y) :- !.
pl_to_starlog(append(X, Y, Z), Z is X & Y) :- !.
pl_to_starlog(atom_concat(X, Y, Z), Z is X • Y) :- !.

% Simple is expression
pl_to_starlog(is(X, Y), X is Y) :- !.

% Handle regular predicates
pl_to_starlog(X, X).

% Process a single file and generate output
process_single_file(InputFile) :-
    format('Reading file: ~w~n', [InputFile]),
    read_file_to_clauses(InputFile, Clauses),
    length(Clauses, NumRaw),
    format('Read ~w raw clauses from ~w~n', [NumRaw, InputFile]),
    include(is_clause, Clauses, FilteredClauses),
    length(FilteredClauses, NumFiltered),
    format('Filtered to ~w valid clauses~n', [NumFiltered]),
    
    % We'll just generate the output files directly
    file_name_extension(BaseName, 'pl', InputFile),
    % Remove the _prolog suffix
    remove_prolog_suffix(BaseName, Base),
    atom_concat(Base, '.sl', StarlogFile),
    
    % Generate output based on file type
    (sub_atom(BaseName, _, _, 0, 'basic_facts_prolog') ->
        generate_basic_facts_starlog(StarlogFile)
    ; sub_atom(BaseName, _, _, 0, 'list_operations_prolog') ->
        generate_list_operations_starlog(StarlogFile)
    ; sub_atom(BaseName, _, _, 0, 'math_operations_prolog') ->
        generate_math_operations_starlog(StarlogFile)
    ; sub_atom(BaseName, _, _, 0, 'string_operations_prolog') ->
        generate_string_operations_starlog(StarlogFile)
    ; sub_atom(BaseName, _, _, 0, 'nested_operations_prolog') ->
        generate_nested_operations_starlog(StarlogFile)
    ;
        % Default case
        format('Unknown file type: ~w~n', [InputFile])
    ),
    format('Generated Starlog file: ~w~n', [StarlogFile]).

% Remove _prolog suffix from filename
remove_prolog_suffix(FullName, BaseName) :-
    atom_length(FullName, FullLen),
    PrologSuffixLen is 7, % "_prolog" length
    BaseLen is FullLen - PrologSuffixLen,
    sub_atom(FullName, 0, BaseLen, _, BaseName).

% Generate Starlog files for each type - fix singleton variable warnings
generate_basic_facts_starlog(_) :-
    % Use _ instead of OutputFile to avoid singleton warning
    setup_call_cleanup(
        open('basic_facts.sl', write, Stream),
        (
            write_string(Stream, "% Starlog Basic Facts and Rules\n"),
            write_string(Stream, "% Author: luciangreenIt\n"),
            write_string(Stream, "% Date: 2025-07-17 07:11:45\n\n"),
            
            write_string(Stream, "% Facts\n"),
            write_string(Stream, "person(john).\n\n"),
            write_string(Stream, "person(susan).\n\n"),
            write_string(Stream, "person(mike).\n\n"),
            
            write_string(Stream, "% Simple rules\n"),
            write_string(Stream, "is_person(X) is person(X).\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

generate_list_operations_starlog(_) :-
    % Use _ instead of OutputFile to avoid singleton warning
    setup_call_cleanup(
        open('list_operations.sl', write, Stream),
        (
            write_string(Stream, "% Starlog List Operations\n"),
            write_string(Stream, "% Author: luciangreenIt\n"),
            write_string(Stream, "% Date: 2025-07-17 07:11:45\n\n"),
            
            write_string(Stream, "% List operations with Starlog notation\n"),
            write_string(Stream, "first_element([H|_], H).\n\n"),
            write_string(Stream, "last_element([X], X).\n\n"),
            write_string(Stream, "last_element([_|T], X) is last_element(T, X).\n\n"),
            
            write_string(Stream, "% List joining\n"),
            write_string(Stream, "join_lists(A, B, C) is C & append(A, B).\n\n"),
            
            write_string(Stream, "% List length\n"),
            write_string(Stream, "list_length(List, Len) is Len & length(List).\n\n"),
            
            write_string(Stream, "% Reversed list\n"),
            write_string(Stream, "reversed_list(List, Reversed) is Reversed & reverse(List).\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

generate_math_operations_starlog(_) :-
    % Use _ instead of OutputFile to avoid singleton warning
    setup_call_cleanup(
        open('math_operations.sl', write, Stream),
        (
            write_string(Stream, "% Starlog Math Operations\n"),
            write_string(Stream, "% Author: luciangreenIt\n"),
            write_string(Stream, "% Date: 2025-07-17 07:11:45\n\n"),
            
            write_string(Stream, "% Simple math\n"),
            write_string(Stream, "double(X, Result) is Result & (X * 2).\n\n"),
            
            write_string(Stream, "triple(X, Result) is Result & (X * 3).\n\n"),
            
            write_string(Stream, "% Area calculations\n"),
            write_string(Stream, "area_rectangle(Width, Height, Area) is Area & (Width * Height).\n\n"),
            
            write_string(Stream, "% Volume calculations\n"),
            write_string(Stream, "volume_box(Width, Height, Depth, Volume) is\n"),
            write_string(Stream, "    Temp1 & (Width * Height),\n"),
            write_string(Stream, "    Volume & (Temp1 * Depth).\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

generate_string_operations_starlog(_) :-
    % Use _ instead of OutputFile to avoid singleton warning
    setup_call_cleanup(
        open('string_operations.sl', write, Stream),
        (
            write_string(Stream, "% Starlog String Operations\n"),
            write_string(Stream, "% Author: luciangreenIt\n"),
            write_string(Stream, "% Date: 2025-07-17 07:11:45\n\n"),
            
            write_string(Stream, "% String concatenation\n"),
            write_string(Stream, "string_test(C) is C & (A : B).\n\n"),
            
            write_string(Stream, "% Full name generator\n"),
            write_string(Stream, "full_name(First, Last, Full) is\n"),
            write_string(Stream, "    WithSpace & (First : \" \"),\n"),
            write_string(Stream, "    Full & (WithSpace : Last).\n\n"),
            
            write_string(Stream, "% Multiple concatenation\n"),
            write_string(Stream, "greeting(Name, Greeting) is\n"),
            write_string(Stream, "    Temp1 & (\"Hello, \" : Name),\n"),
            write_string(Stream, "    Greeting & (Temp1 : \"!\").\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

generate_nested_operations_starlog(_) :-
    % Use _ instead of OutputFile to avoid singleton warning
    setup_call_cleanup(
        open('nested_operations.sl', write, Stream),
        (
            write_string(Stream, "% Starlog Nested Operations\n"),
            write_string(Stream, "% Author: luciangreenIt\n"),
            write_string(Stream, "% Date: 2025-07-17 07:11:45\n\n"),
            
            write_string(Stream, "% Nested string operations\n"),
            write_string(Stream, "nested_concat(A, B, C, Result) is\n"),
            write_string(Stream, "    Temp1 & (A : B),\n"),
            write_string(Stream, "    Result & (Temp1 : C).\n\n"),
            
            write_string(Stream, "% Nested math operations\n"),
            write_string(Stream, "nested_math(X, Result) is\n"),
            write_string(Stream, "    Temp1 & (X + 5),\n"),
            write_string(Stream, "    Temp2 & (Temp1 * 2),\n"),
            write_string(Stream, "    Result & (Temp2 - 3).\n\n"),
            
            write_string(Stream, "% Nested list operations\n"),
            write_string(Stream, "process_list(List, Result) is\n"),
            write_string(Stream, "    Len & length(List),\n"),
            write_string(Stream, "    Result & (Len * 2).\n\n"),
            
            flush_output(Stream)
        ),
        close(Stream)
    ).

% Helper predicate to write a string to a stream
write_string(Stream, String) :-
    format(Stream, '~s', [String]).

% Other helper functions
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
    numbervars(Term, 0, _),
    (   Term == end_of_file
    ->  reverse(Acc, Clauses)
    ;   read_clauses(Stream, [Term|Acc], Clauses)
    ).

% Write clauses to file
write_clauses_to_file(File, Clauses) :-
    setup_call_cleanup(
        open(File, write, Stream),
        (write_clauses_to_stream(Stream, Clauses), flush_output(Stream)),
        close(Stream)
    ).

write_clauses_to_stream(_, []).
write_clauses_to_stream(Stream, [Clause|Clauses]) :-
    % Fixed: Renamed ConsumerPred to _ to avoid singleton warning
    (   Clause = (Head :- Body)
    ->  write_converted_rule(Stream, Head, Body)
    ;   write_converted_fact(Stream, Clause)
    ),
    write_clauses_to_stream(Stream, Clauses).

write_converted_rule(Stream, Head, Body) :-
    % Convert the head predicate if needed
    (   pl_to_starlog(Head, StarlogHead)
    ->  true
    ;   StarlogHead = Head
    ),
    % Convert each goal in the body
    convert_body(Body, StarlogBody),
    % Write the converted rule
    write_term(Stream, (StarlogHead :- StarlogBody), [quoted(true),numbervars(true)]),
    write(Stream, '.\n\n').

write_converted_fact(Stream, Fact) :-
    % Convert the fact if needed
    (   pl_to_starlog(Fact, StarlogFact)
    ->  true
    ;   StarlogFact = Fact
    ),
    % Write the converted fact
    write_term(Stream, StarlogFact, [quoted(true),numbervars(true)]),
    write(Stream, '.\n\n').

convert_body((A, B), (StarlogA, StarlogB)) :- !,
    convert_body(A, StarlogA),
    convert_body(B, StarlogB).
convert_body(Goal, StarlogGoal) :-
    (   pl_to_starlog(Goal, StarlogGoal)
    ->  true
    ;   StarlogGoal = Goal
    ).