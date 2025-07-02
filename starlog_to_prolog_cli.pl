:- module(starlog_to_prolog_cli, [main/0]).
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
    format('Date/Time: 2025-07-02 15:24:50 UTC~n'),
    format('User: luciangreenGo~n'),
    format('Working in directory: ~w~n', [CWD]),
    
    % Look for Starlog files in the current directory
    directory_files('.', AllFiles),
    exclude(hidden_or_special, AllFiles, Files),
    include(is_starlog_file, Files, StarlogFiles),
    ( StarlogFiles = [] ->
        format('No _starlog.pl files found for conversion~n')
    ; length(StarlogFiles, NumFiles),
      format('Found ~w Starlog files to process: ~w~n', [NumFiles, StarlogFiles]),
      process_starlog_files(StarlogFiles)
    ),
    format('~n=== Starlog to Prolog Conversion Complete ===~n').

hidden_or_special(File) :- sub_atom(File, 0, 1, _, '.').
hidden_or_special('..').
hidden_or_special('.').

is_starlog_file(File) :-
    file_name_extension(Base, 'pl', File),
    sub_atom(Base, _, _, 0, '_starlog').

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
    maplist(starlog_to_pl_with_decompression, Clauses, PrologClauses),
    length(PrologClauses, NumConverted),
    format('Converted ~w clauses to Prolog with decompression~n', [NumConverted]),
    maplist(rename_vars_pretty, PrologClauses, PrettyClauses),
    
    % Extract base filename and create output path
    file_name_extension(Base, 'pl', InputFile),
    % The output file should be named from the original .pl file
    % First, check if the base name ends with '_starlog'
    (   sub_atom(Base, BeforePos, 8, 0, '_starlog')
    ->  sub_atom(Base, 0, BeforePos, _, OrigBase),  % Extract the original base name
        atom_concat(OrigBase, '_starlog_prolog.pl', OutputFile)
    ;   atom_concat(Base, '_prolog.pl', OutputFile)
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

% --- Enhanced Converter: Starlog clause to Prolog clause with decompression ---
starlog_to_pl_with_decompression((Var is Func :- Body), (Head :- PBody)) :- 
    !,
    reconstruct_prolog_head(Var, Func, Head),
    starlog_body_to_pl_decompressed(Body, PBody).
starlog_to_pl_with_decompression((Head :- Body), (Head :- PBody)) :- 
    !, 
    starlog_body_to_pl_decompressed(Body, PBody).
starlog_to_pl_with_decompression(Fact, Fact).

% Reconstruct a Prolog predicate head from Starlog format
reconstruct_prolog_head(OutputVar, Func, Head) :-
    Func =.. [Pred|Args],
    get_output_position(Pred, OutPos),
    insert_at_position(Args, OutPos, OutputVar, AllArgs),
    Head =.. [Pred|AllArgs].

% Get the output position for known predicates
get_output_position(remove_trailing_white_space, 2).
get_output_position(split12, 4).
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

% Decompress Starlog body by flattening nested expressions
starlog_body_to_pl_decompressed(Body, DecompressedBody) :-
    flatten_conjunction(Body, Goals),
    maplist(decompress_goal, Goals, DecompressedGoalLists),
    append(DecompressedGoalLists, FlatGoals),
    rebuild_conjunction(FlatGoals, DecompressedBody).

% Decompress a single goal, potentially expanding nested expressions
decompress_goal(Goal, Goals) :-
    (   decompress_nested_goal(Goal, Goals)
    ->  format('  Decompressed nested goal: ~w~n', [Goal])
    ;   starlog_goal_to_pl(Goal, PlGoal),
        Goals = [PlGoal]
    ).

% Decompress nested expressions in goals - BUT NOT simple Starlog built-ins
decompress_nested_goal(Goal, Goals) :-
    Goal =.. [Pred|InArgs],
    \+ is_simple_starlog_builtin(Goal),  % Don't decompose simple Starlog built-ins
    decompose_nested_args(InArgs, [], SimpleArgs, PreGoals),
    PreGoals \= [],
    NewGoal =.. [Pred|SimpleArgs],
    starlog_goal_to_pl(NewGoal, PlGoal),
    append(PreGoals, [PlGoal], Goals).

% Check if this is a simple Starlog built-in that shouldn't be decomposed
is_simple_starlog_builtin((_ is (_ : _))).
is_simple_starlog_builtin((_ is (_ & _))).
is_simple_starlog_builtin((_ is (_ • _))).
is_simple_starlog_builtin((_ is string_length(_))).
is_simple_starlog_builtin((_ is atom_length(_))).
is_simple_starlog_builtin((_ is number_string(_))).
is_simple_starlog_builtin((_ is string_chars(_))).
is_simple_starlog_builtin((_ is atom_chars(_))).
is_simple_starlog_builtin((_ is sub_string(_,_,_,_))).
is_simple_starlog_builtin((_ is reverse(_))).
is_simple_starlog_builtin((_ is length_1(_))).
is_simple_starlog_builtin((_ is member_1(_))).
is_simple_starlog_builtin((_ is head(_))).
is_simple_starlog_builtin((_ is tail(_))).
is_simple_starlog_builtin((_ is delete(_,_))).
is_simple_starlog_builtin((_ is wrap(_))).
is_simple_starlog_builtin((_ is unwrap(_))).
is_simple_starlog_builtin((_ is string_to_number(_))).
is_simple_starlog_builtin((_ is random(_))).
is_simple_starlog_builtin((_ is date)).
is_simple_starlog_builtin((_ is ceiling(_))).
is_simple_starlog_builtin((_ is sqrt(_))).
is_simple_starlog_builtin((_ is round(_))).
is_simple_starlog_builtin((_ is findall(_,_))).
is_simple_starlog_builtin((_ is string_from_file(_))).
is_simple_starlog_builtin((_ is maplist(_,_))).
is_simple_starlog_builtin((_ is sort(_))).
is_simple_starlog_builtin((_ is intersection(_,_))).
is_simple_starlog_builtin((_ is read_string(_,_,_,_))).
is_simple_starlog_builtin((_ is atom_string(_))).

% Decompose arguments, extracting nested expressions into separate goals
decompose_nested_args([], SimpleArgs, SimpleArgs, []).
decompose_nested_args([Arg|Args], AccSimple, SimpleArgs, AllPreGoals) :-
    (   is_truly_nested_expression(Arg)
    ->  decompose_nested_expression(Arg, SimpleArg, PreGoals),
        append(AccSimple, [SimpleArg], NewAccSimple),
        decompose_nested_args(Args, NewAccSimple, SimpleArgs, RestPreGoals),
        append(PreGoals, RestPreGoals, AllPreGoals)
    ;   append(AccSimple, [Arg], NewAccSimple),
        decompose_nested_args(Args, NewAccSimple, SimpleArgs, AllPreGoals)
    ).

% Check if an expression is truly nested (compound but not a simple Starlog operation)
is_truly_nested_expression(Expr) :-
    compound(Expr),
    \+ is_simple_starlog_operation(Expr),
    \+ is_simple_compound(Expr).

% Simple Starlog operations that don't need further decomposition
is_simple_starlog_operation(string_length(_)).
is_simple_starlog_operation(atom_length(_)).
is_simple_starlog_operation(number_string(_)).
is_simple_starlog_operation(string_chars(_)).
is_simple_starlog_operation(atom_chars(_)).
is_simple_starlog_operation(sub_string(_,_,_,_)).
is_simple_starlog_operation(reverse(_)).
is_simple_starlog_operation(length_1(_)).
is_simple_starlog_operation(member_1(_)).
is_simple_starlog_operation(head(_)).
is_simple_starlog_operation(tail(_)).
is_simple_starlog_operation(delete(_,_)).
is_simple_starlog_operation(wrap(_)).
is_simple_starlog_operation(unwrap(_)).
is_simple_starlog_operation(string_to_number(_)).
is_simple_starlog_operation(random(_)).
is_simple_starlog_operation(ceiling(_)).
is_simple_starlog_operation(sqrt(_)).
is_simple_starlog_operation(round(_)).
is_simple_starlog_operation(findall(_,_)).
is_simple_starlog_operation(string_from_file(_)).
is_simple_starlog_operation(maplist(_,_)).
is_simple_starlog_operation(sort(_)).
is_simple_starlog_operation(intersection(_,_)).
is_simple_starlog_operation(read_string(_,_,_,_)).
is_simple_starlog_operation(atom_string(_)).
is_simple_starlog_operation(is_space(_)).

% Simple compounds that don't need decomposition
is_simple_compound((_ : _)).
is_simple_compound((_ & _)).
is_simple_compound((_ • _)).

% Decompose a nested expression into a variable and prerequisite goals
decompose_nested_expression(Expr, Var, Goals) :-
    gensym('_G', Var),
    create_goal_for_expression(Expr, Var, Goal),
    extract_subexpressions(Expr, SubGoals),
    append(SubGoals, [Goal], Goals).

% Create a goal that computes the given expression
create_goal_for_expression(Expr, Var, Goal) :-
    (   functor(Expr, Functor, _),
        (   Functor = length_1
        ->  Expr =.. [_|Args],
            Goal =.. [length, Args, Var]
        ;   Functor = member_1
        ->  Expr =.. [_|Args],
            Goal =.. [member, Args, Var]
        ;   Expr =.. [Functor|Args],
            append(Args, [Var], NewArgs),
            Goal =.. [Functor|NewArgs]
        )
    ;   Goal = (Var = Expr)
    ).

% Extract subexpressions that need to be computed first
extract_subexpressions(Expr, Goals) :-
    compound(Expr),
    Expr =.. [_|Args],
    extract_subexprs_from_args(Args, Goals).

extract_subexprs_from_args([], []).
extract_subexprs_from_args([Arg|Args], AllGoals) :-
    (   is_truly_nested_expression(Arg)
    ->  decompose_nested_expression(Arg, _, ArgGoals),
        extract_subexprs_from_args(Args, RestGoals),
        append(ArgGoals, RestGoals, AllGoals)
    ;   extract_subexprs_from_args(Args, AllGoals)
    ).

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
starlog_goal_to_pl(X is (Y : Z), string_concat(Y, Z, X)) :- !, safe_concat_args(Y, Z).
starlog_goal_to_pl(X is (Y & Z), append(Y, Z, X)) :- !.
starlog_goal_to_pl(X is (Y • Z), atom_concat(Y, Z, X)) :- !.
starlog_goal_to_pl(X is string_length(Y), string_length(Y, X)) :- !.
starlog_goal_to_pl(X is number_string(Y), number_string(Y, X)) :- !.
starlog_goal_to_pl(X is atom_length(Y), atom_length(Y, X)) :- !.
starlog_goal_to_pl(X is sub_string(A,B,C,D), sub_string(A,B,C,D,X)) :- !.
starlog_goal_to_pl(X is string_chars(Y), string_chars(Y, X)) :- !.
starlog_goal_to_pl(X is atom_chars(Y), atom_chars(Y, X)) :- !.
starlog_goal_to_pl(X is atom_string(Y), atom_string(Y, X)) :- !.

% List operations
starlog_goal_to_pl(X is reverse(Y), reverse(Y, X)) :- !.
starlog_goal_to_pl(X is length_1(Y), length(Y, X)) :- !.
starlog_goal_to_pl(X is member_1(Y), member(Y, X)) :- !.
starlog_goal_to_pl(X is head(Y), head(Y, X)) :- !.
starlog_goal_to_pl(X is tail(Y), tail(Y, X)) :- !.
starlog_goal_to_pl(X is delete(Y,Z), delete(Y,Z,X)) :- !.
starlog_goal_to_pl(X is wrap(Y), wrap(Y, X)) :- !.
starlog_goal_to_pl(X is unwrap(Y), unwrap(Y, X)) :- !.
starlog_goal_to_pl(X is maplist(Y,Z), maplist(Y,Z,X)) :- !.
starlog_goal_to_pl(X is sort(Y), sort(Y, X)) :- !.
starlog_goal_to_pl(X is intersection(Y,Z), intersection(Y,Z,X)) :- !.

% Math operations
starlog_goal_to_pl(X is Y, is(X, Y)) :- compound(Y), \+ is_simple_starlog_operation(Y), !.
starlog_goal_to_pl(X = Y, =(X, Y)) :- !.
starlog_goal_to_pl(X is string_to_number(Y), string_to_number(Y, X)) :- !.
starlog_goal_to_pl(X is random(Y), random(Y, X)) :- !.
starlog_goal_to_pl(X is ceiling(Y), ceiling(Y, X)) :- !.
starlog_goal_to_pl(X is sqrt(Y), sqrt(Y, X)) :- !.
starlog_goal_to_pl(X is round(Y), round(Y, X)) :- !.

% Other operations
starlog_goal_to_pl(X is date, date(X)) :- !.
starlog_goal_to_pl(X is findall(Y,Z), findall(Y,Z,X)) :- !.
starlog_goal_to_pl(X is string_from_file(Y), string_from_file(Y, X)) :- !.
starlog_goal_to_pl(X is read_string(A,B,C,D), read_string(A,B,C,D,X)) :- !.
starlog_goal_to_pl(is_space(X), is_space(X)) :- !. % Keep is_space predicate as is
starlog_goal_to_pl(Goal, call(Goal)) :- is_truly_nested_expression(Goal), !.  % Handle call as last resort

starlog_goal_to_pl(true, true) :- !.
starlog_goal_to_pl(Goal, Goal).

safe_concat_args(Y, Z) :- safe_concat_arg(Y), safe_concat_arg(Z).
safe_concat_arg(Arg) :- var(Arg), !.
safe_concat_arg(Arg) :- atom(Arg), !.
safe_concat_arg(Arg) :- string(Arg), !.

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
