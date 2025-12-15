:- module(starlog_to_prolog_cli, [main/0]).
:- use_module(var_utils).

% Define operators for Starlog syntax
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
    format('Date/Time: 2025-07-02 00:25:18 UTC~n'),
    format('User: luciangreenPlease~n'),
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
    catch(process_single_file(File), Error,
          format('Error processing ~w: ~w~n', [File, Error])),
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
    % Append "_prolog" to the base name (without removing _starlog)
    atom_concat(Base, '_prolog.pl', OutputFile),
    
    format('Writing to: ~w~n', [OutputFile]),
    write_clauses_to_file(OutputFile, PrettyClauses),
    format('Wrote converted file: ~w~n', [OutputFile]).

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

% --- Enhanced Converter: Starlog clause to Prolog clause with decompression ---
% User-defined predicates keep their original head structure
% Only built-in predicates in the body are converted from Starlog "is" syntax
starlog_to_pl_with_decompression((Head :- Body), (Head :- PBody)) :- 
    !, 
    starlog_body_to_pl_decompressed(Body, PBody).
starlog_to_pl_with_decompression(Fact, Fact).

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

% Decompress nested expressions in goals - BUT NOT simple Starlog built-ins or standard Prolog
decompress_nested_goal(Goal, Goals) :-
    Goal =.. [Pred|InArgs],
    \+ is_simple_starlog_builtin(Goal),  % Don't decompose simple Starlog built-ins
    \+ is_standard_prolog_goal(Goal),     % Don't decompose standard Prolog goals
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
is_simple_starlog_builtin((_ is length_1(_))). % length'1(_)))
is_simple_starlog_builtin((_ is member_1(_))). % member'1(_)))
is_simple_starlog_builtin((_ is findall_until_fail(_,_,_))).
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

% Check if this is a standard Prolog goal that shouldn't be decomposed
% Standard Prolog is/2 with non-Starlog-operation right-hand side should not be decomposed
% Cut is used here to commit once we've identified it's an is/2 goal, preventing unnecessary
% checking of other is_standard_prolog_goal clauses for efficiency
is_standard_prolog_goal((_ is Expr)) :- 
    \+ is_simple_starlog_operation(Expr),
    % Also check it's not a recognized Starlog operator pattern
    \+ (compound(Expr), functor(Expr, Op, _), is_starlog_operator(Op)),
    !.
% Standard Prolog predicates that are not Starlog built-ins
is_standard_prolog_goal(member(_, _)).
is_standard_prolog_goal(append(_, _, _)).
is_standard_prolog_goal(length(_, _)).
is_standard_prolog_goal(_ = _).
is_standard_prolog_goal(true).
is_standard_prolog_goal(fail).
is_standard_prolog_goal(!).

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
    \+ is_simple_compound(Expr),
    \+ is_list_structure(Expr),  % Don't treat list structures as nested expressions
    functor(Expr, Functor, _),
    Functor \= '$VAR'.  % Don't treat numbered variables as nested expressions

% Check if this is a list structure (internal representation of lists)
% Note: [] handles the list syntax sugar while '[]' handles the atom representation
is_list_structure([]).          % Empty list syntax sugar
is_list_structure([_|_]).       % Non-empty list syntax sugar
is_list_structure('[]').        % Empty list as atom (after read_term processing)
is_list_structure('.'(_, _)).   % Standard Prolog list cell representation
is_list_structure('[|]'(_, _)). % Alternative list representation (SWI-Prolog, after numbervars)

% Simple Starlog operations that don't need further decomposition
is_simple_starlog_operation(string_length(_)).
is_simple_starlog_operation(atom_length(_)).
is_simple_starlog_operation(number_string(_)).
is_simple_starlog_operation(string_chars(_)).
is_simple_starlog_operation(atom_chars(_)).
is_simple_starlog_operation(sub_string(_,_,_,_)).
is_simple_starlog_operation(reverse(_)).
is_simple_starlog_operation(length_1(_)). % length'1(_))
is_simple_starlog_operation(member_1(_)). % member'1(_))
is_simple_starlog_operation(findall_until_fail(_,_,_)).
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

% Simple compounds that don't need decomposition
is_simple_compound((_ : _)).
is_simple_compound((_ & _)).
is_simple_compound((_ • _)).

% Starlog operators (centralized definition)
is_starlog_operator(':').
is_starlog_operator('&').
is_starlog_operator('•').

% Decompose a nested expression into a variable and prerequisite goals
decompose_nested_expression(Expr, Var, Goals) :-
    gensym('_G', Var),
    create_goal_for_expr(Expr, Var, Goal),
    extract_subexpressions(Expr, SubGoals),
    append(SubGoals, [Goal], Goals).

% Create a goal that computes the given expression
create_goal_for_expr(Expr, Var, Goal) :-
    (   functor(Expr, Functor, _),
        (   Functor = length_1 % length'1
        ->  Expr =.. [_|Args],
            append(Args, [Var], AllArgs),
            Goal =.. [length|AllArgs]
        ;   Functor = member_1 % member'1
        ->  Expr =.. [_|Args],
            append(Args, [Var], AllArgs),
            Goal =.. [member|AllArgs]
        ;   Functor = findall_until_fail
        ->  Expr =.. [_, A, B, C],
            Goal =.. [findall_until_fail, A, B, C, Var]
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
starlog_goal_to_pl(X is length_1(Y), length(Y, X)) :- !. % length'1(Y)
starlog_goal_to_pl(X is member_1(Y), member(Y, X)) :- !. % member'1(Y)
starlog_goal_to_pl(X is findall_until_fail(A,B,C), findall_until_fail(A,B,C,X)) :- !.
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

% Fallthrough: pass non-Starlog predicates through unchanged
starlog_goal_to_pl(true, true) :- !.
starlog_goal_to_pl(Goal, Goal).

safe_concat_args(Y, Z) :- safe_concat_arg(Y), safe_concat_arg(Z).
safe_concat_arg(Arg) :- var(Arg), !.
safe_concat_arg(Arg) :- atom(Arg), !.
safe_concat_arg(Arg) :- string(Arg), !.
safe_concat_arg('$VAR'(_)) :- !.  % Allow numbered variables from numbervars/3

write_clauses_to_file(File, Clauses) :-
    setup_call_cleanup(
        open(File, write, Stream),
        (write_file_header(Stream), write_clauses(Stream, Clauses), flush_output(Stream)),
        close(Stream)
    ).

write_file_header(Stream) :-
    format(Stream, '% Generated by Starlog to Prolog Converter~n', []),
    format(Stream, '% Date/Time: 2025-07-02 00:25:18 UTC~n', []),
    format(Stream, '% User: luciangreenPlease~n', []),
    format(Stream, '% Decompression applied where possible~n~n', []).

write_clauses(_, []).
write_clauses(Stream, [Clause|Clauses]) :-
    ( Clause = (Head :- true) ->
        write_term(Stream, Head, [quoted(true), numbervars(true)]), write(Stream, '.\n')
    ; Clause = (Head :- Body) ->
        write_term(Stream, (Head :- Body), [quoted(true), numbervars(true)]), write(Stream, '.\n')
    ;   write_term(Stream, Clause, [quoted(true), numbervars(true)]), write(Stream, '.\n')
    ),
    write_clauses(Stream, Clauses).

write_clauses(Stream, [Clause|Clauses]) :-
    ( Clause = (Head --> true) ->
        write_term(Stream, Head, [quoted(true), numbervars(true)]), write(Stream, '.\n')
    ; Clause = (Head --> Body) ->
        write_term(Stream, (Head --> Body), [quoted(true), numbervars(true)]), write(Stream, '.\n')
    ;   write_term(Stream, Clause, [quoted(true), numbervars(true)]), write(Stream, '.\n')
    ),
    write_clauses(Stream, Clauses).
