:- module(prolog_to_starlog_cli, [main/0]).
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
    format('Date/Time: 2025-07-02 00:25:18 UTC~n'),
    format('User: luciangreenPlease~n'),
    format('Working in directory: ~w~n', [CWD]),
    
    % Look for files in the current directory
    directory_files('.', AllFiles),
    exclude(hidden_or_special, AllFiles, Files),
    include(is_source_file, Files, PrologFiles),
    length(PrologFiles, NumFiles),
    format('Found ~w Prolog files to process: ~w~n', [NumFiles, PrologFiles]),
    process_prolog_files(PrologFiles),
    format('~n=== Prolog to Starlog Conversion Complete ===~n').

hidden_or_special(File) :- sub_atom(File, 0, 1, _, '.').
hidden_or_special('..').
hidden_or_special('.').

is_source_file(File) :-
    file_name_extension(Base, 'pl', File),
    \+ sub_atom(Base, _, _, 0, '_starlog'),
    \+ sub_atom(Base, _, _, 0, '_cli'),
    Base \= 'var_utils'.

process_prolog_files([]).
process_prolog_files([File|Files]) :-
    format('Processing file: ~w~n', [File]),
    (   catch(process_single_file(File), Error,
              format('Error processing ~w: ~w~n', [File, Error]))
    ->  true
    ;   format('Warning: Failed to process ~w but continuing...~n', [File])
    ),
    process_prolog_files(Files).

process_single_file(InputFile) :-
    read_file_to_clauses(InputFile, Clauses0),
    length(Clauses0, NumRaw),
    format('Read ~w raw clauses from ~w~n', [NumRaw, InputFile]),
    include(is_clause, Clauses0, Clauses),
    length(Clauses, NumFiltered),
    format('Filtered to ~w valid clauses~n', [NumFiltered]),
    maplist(pl_to_starlog_with_compression, Clauses, StarlogClauses),
    length(StarlogClauses, NumConverted),
    format('Converted ~w clauses to Starlog with compression~n', [NumConverted]),
    maplist(rename_vars_pretty, StarlogClauses, PrettyClauses),
    
    % Extract base filename and create output path
    file_name_extension(Base, 'pl', InputFile),
    atom_concat(Base, '_starlog.pl', OutputFile),
    
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

% --- Enhanced Converter: Prolog clause to Starlog clause with compression ---
% User-defined predicates keep their original head structure
% Only built-in predicates in the body are converted to Starlog "is" syntax
pl_to_starlog_with_compression((Head :- Body), (Head :- SBody)) :- 
    !, 
    pl_body_to_starlog_compressed(Body, SBody).
pl_to_starlog_with_compression(Fact, Fact).

% Compress conjunctions by looking for opportunities to nest predicate calls
pl_body_to_starlog_compressed(Body, CompressedBody) :-
    flatten_conjunction(Body, Goals),
    eliminate_duplicate_expressions(Goals, UniqueGoals),
    compress_goals(UniqueGoals, CompressedGoals),
    rebuild_conjunction(CompressedGoals, CompressedBody).

% Eliminate duplicate expressions like (A is f, A is f) -> (A is f)
eliminate_duplicate_expressions([], []).
eliminate_duplicate_expressions([Goal|Goals], [Goal|UniqueGoals]) :-
    exclude(same_assignment(Goal), Goals, FilteredGoals),
    eliminate_duplicate_expressions(FilteredGoals, UniqueGoals).

% Check if two goals are the same assignment (A is X) expressions
same_assignment((Var1 is Expr1), (Var2 is Expr2)) :-
    Var1 == Var2, Expr1 =@= Expr2, !.
same_assignment(_, _) :- fail.

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

% Compress goals by identifying opportunities to nest function calls
compress_goals(Goals, CompressedGoals) :-
    compress_goals_pass(Goals, CompressedGoals).

compress_goals_pass([], []).
compress_goals_pass([Goal|Goals], [CompressedGoal|CompressedGoals]) :-
    (   find_compression_opportunity(Goal, Goals, CompressedGoal, RemainingGoals)
    ->  format('  Compressed: ~w with following goals~n', [Goal]),
        compress_goals_pass(RemainingGoals, CompressedGoals)
    ;   pl_goal_to_starlog(Goal, CompressedGoal),
        compress_goals_pass(Goals, CompressedGoals)
    ).

% Find opportunities to compress consecutive goals
find_compression_opportunity(FirstGoal, RestGoals, CompressedGoal, NewRestGoals) :-
    % Look for pattern: builtin_pred(Args, Output), next_pred(..., Output, ...)
    is_compressible_builtin(FirstGoal, Output),
    select_goal_using_var(RestGoals, Output, SecondGoal, NewRestGoals),
    compress_two_goals(FirstGoal, SecondGoal, Output, CompressedGoal).

% Check if a goal is a compressible built-in predicate
is_compressible_builtin(string_concat(_, _, Output), Output).
is_compressible_builtin(append(_, _, Output), Output).
is_compressible_builtin(atom_concat(_, _, Output), Output).
is_compressible_builtin(string_length(_, Output), Output).
is_compressible_builtin(atom_length(_, Output), Output).
is_compressible_builtin(number_string(_, Output), Output).
is_compressible_builtin(string_chars(_, Output), Output).
is_compressible_builtin(atom_chars(_, Output), Output).
is_compressible_builtin(sub_string(_, _, _, _, Output), Output).
is_compressible_builtin(length(_, Output), Output).
is_compressible_builtin(member(_, Output), Output).
is_compressible_builtin(reverse(_, Output), Output).
is_compressible_builtin(wrap(_, Output), Output).
is_compressible_builtin(unwrap(_, Output), Output).
is_compressible_builtin(head(_, Output), Output).
is_compressible_builtin(tail(_, Output), Output).
is_compressible_builtin(delete(_, _, Output), Output).
is_compressible_builtin(string_to_number(_, Output), Output).
is_compressible_builtin(random(_, Output), Output).
is_compressible_builtin(ceiling(_, Output), Output).
is_compressible_builtin(date(Output), Output).
is_compressible_builtin(sqrt(_, Output), Output).
is_compressible_builtin(round(_, Output), Output).
is_compressible_builtin(findall(_, _, Output), Output).
is_compressible_builtin(string_from_file(_, Output), Output).
is_compressible_builtin(maplist(_, _, Output), Output).
is_compressible_builtin(sort(_, Output), Output).
is_compressible_builtin(intersection(_, _, Output), Output).
is_compressible_builtin(read_string(_, _, _, _, Output), Output).
is_compressible_builtin(atom_string(_, Output), Output).
is_compressible_builtin(call(Output), Output).

% Select a goal that uses the given variable and remove it from the list
select_goal_using_var([Goal|Goals], Var, Goal, Goals) :-
    goal_uses_var(Goal, Var),
    var_used_only_once([Goal|Goals], Var).
select_goal_using_var([Goal|Goals], Var, SelectedGoal, [Goal|RestGoals]) :-
    select_goal_using_var(Goals, Var, SelectedGoal, RestGoals).

% Check if a goal uses a specific variable
goal_uses_var(Goal, Var) :-
    term_variables(Goal, Vars),
    member(Var, Vars).

% Check if a variable is used only once in the remaining goals
var_used_only_once(Goals, Var) :-
    findall(G, (member(G, Goals), goal_uses_var(G, Var)), Uses),
    length(Uses, 1).

% Compress two goals into one by nesting the first into the second
compress_two_goals(FirstGoal, SecondGoal, SharedVar, CompressedGoal) :-
    pl_goal_to_starlog(FirstGoal, FirstStarlog),
    extract_starlog_expression(FirstStarlog, SharedVar, NestedExpr),
    substitute_var_in_goal(SecondGoal, SharedVar, NestedExpr, CompressedGoal).

% Extract the expression from a Starlog 'is' statement
extract_starlog_expression((Var is Expr), Var, Expr) :- !.
extract_starlog_expression((Var = Expr), Var, Expr) :- !.
extract_starlog_expression(Goal, _, Goal).

% Substitute a variable in a goal with an expression
substitute_var_in_goal(Goal, Var, Expr, NewGoal) :-
    Goal =.. [Pred|Args],
    substitute_var_in_args(Args, Var, Expr, NewArgs),
    NewGoal =.. [Pred|NewArgs].

substitute_var_in_args([], _, _, []).
substitute_var_in_args([Arg|Args], Var, Expr, [NewArg|NewArgs]) :-
    (   Arg == Var
    ->  NewArg = Expr
    ;   NewArg = Arg
    ),
    substitute_var_in_args(Args, Var, Expr, NewArgs).

% Convert individual Prolog goals to Starlog
pl_goal_to_starlog(string_concat(A,B,C), (C is A : B)) :- !.
pl_goal_to_starlog(append(A,B,C), (C is A & B)) :- !.
pl_goal_to_starlog(atom_concat(A,B,C), (C is A • B)) :- !.
pl_goal_to_starlog(string_length(A,B), (B is string_length(A))) :- !.
pl_goal_to_starlog(number_string(A,B), (B is number_string(A))) :- !.
pl_goal_to_starlog(atom_length(A,B), (B is atom_length(A))) :- !.
pl_goal_to_starlog(sub_string(A,B,C,D,E), (E is sub_string(A,B,C,D))) :- !.
pl_goal_to_starlog(string_chars(A,B), (B is string_chars(A))) :- !.
pl_goal_to_starlog(atom_chars(A,B), (B is atom_chars(A))) :- !.

% List operations
pl_goal_to_starlog(length(A,B), (B is length_1(A))) :- !.
pl_goal_to_starlog(member(A,B), (B is member_1(A))) :- !.
pl_goal_to_starlog(reverse(A,B), (B is reverse(A))) :- !.
pl_goal_to_starlog(findall_until_fail(A,B,C,D), (D is findall_until_fail(A,B,C))) :- !.
pl_goal_to_starlog(head(A,B), (B is head(A))) :- !.
pl_goal_to_starlog(tail(A,B), (B is tail(A))) :- !.
pl_goal_to_starlog(delete(A,B,C), (C is delete(A,B))) :- !.
pl_goal_to_starlog(wrap(A,B), (B is wrap(A))) :- !.
pl_goal_to_starlog(unwrap(A,B), (B is unwrap(A))) :- !.
pl_goal_to_starlog(maplist(A,B,C), (C is maplist(A,B))) :- !.
pl_goal_to_starlog(sort(A,B), (B is sort(A))) :- !.
pl_goal_to_starlog(intersection(A,B,C), (C is intersection(A,B))) :- !.

% Math operations
pl_goal_to_starlog(is(A,B), (A is B)) :- !.
pl_goal_to_starlog(=(A,B), (A = B)) :- !.
pl_goal_to_starlog(string_to_number(A,B), (B is string_to_number(A))) :- !.
pl_goal_to_starlog(random(A,B), (B is random(A))) :- !.
pl_goal_to_starlog(ceiling(A,B), (B is ceiling(A))) :- !.
pl_goal_to_starlog(sqrt(A,B), (B is sqrt(A))) :- !.
pl_goal_to_starlog(round(A,B), (B is round(A))) :- !.

% Other operations
pl_goal_to_starlog(date(A), (A is date)) :- !.
pl_goal_to_starlog(findall(A,B,C), (C is findall(A,B))) :- !.
pl_goal_to_starlog(string_from_file(A,B), (B is string_from_file(A))) :- !.
pl_goal_to_starlog(read_string(A,B,C,D,E), (E is read_string(A,B,C,D))) :- !.
pl_goal_to_starlog(atom_string(A,B), (B is atom_string(A))) :- !.
pl_goal_to_starlog(call(A), A) :- !.

pl_goal_to_starlog(true, true) :- !.
pl_goal_to_starlog(Goal, Goal).

write_clauses_to_file(File, Clauses) :-
    setup_call_cleanup(
        open(File, write, Stream),
        (write_file_header(Stream), write_clauses(Stream, Clauses), flush_output(Stream)),
        close(Stream)
    ).

write_file_header(Stream) :-
    format(Stream, '% Generated by Prolog to Starlog Converter~n', []),
    format(Stream, '% Date/Time: 2025-07-02 00:25:18 UTC~n', []),
    format(Stream, '% User: luciangreenPlease~n', []),
    format(Stream, '% Compression applied where possible~n~n', []).

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
