:- module(starlog_to_prolog, [main/0]).
:- use_module(var_utils).
:- use_module(library(charsio)).
%:-include('../listprologinterpreter/listprolog.pl').

:-dynamic new_var_sl2p888/1.
:-dynamic free_vars_sl2p888/1.

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
    format('~n=== Starlog to Prolog Converter ===~n', []),
    format('Date/Time: 2025-07-02 00:25:18 UTC~n', []),
    format('User: luciangreenPlease~n', []),
    format('Working in directory: ~w~n', [CWD]),
    
    % Look for Starlog files in the current directory
    directory_files('.', AllFiles),
    exclude(hidden_or_special, AllFiles, Files),
    include(is_starlog_file, Files, StarlogFiles),
    ( StarlogFiles = [] ->
        format('No _starlog.pl files found for conversion~n', [])
    ; length(StarlogFiles, NumFiles),
      format('Found ~w Starlog files to process: ~w~n', [NumFiles, StarlogFiles]),
      process_starlog_files(StarlogFiles)
    ),
    format('~n=== Starlog to Prolog Conversion Complete ===~n', []).


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
    %trace,
    maplist(starlog_to_pl_with_decompression, Clauses, PrologClauses),
    length(PrologClauses, NumConverted),
    format('Converted ~w clauses to Prolog with decompression~n', [NumConverted]),
    %trace,
    maplist(rename_vars_pretty, PrologClauses, PrettyClauses1),
    %trace,
    retractall(new_var_sl2p888(_)),
    assertz(new_var_sl2p888(0)),
    retractall(free_vars_sl2p888(_)),
    assertz(free_vars_sl2p888([])),
    %trace,
    %leash(-all),
    transform_list(PrettyClauses1,PrettyClauses),
    %with_output_to(string(S),write_term(PrettyClauses))
    % Extract base filename and create output path
    file_name_extension(Base, 'pl', InputFile),
    % Append "_prolog" to the base name (without removing _starlog)
    atom_concat(Base, '_prolog.pl', OutputFile),
    
    format('Writing to: ~w~n', [OutputFile]),
    write_clauses_to_file(OutputFile, PrettyClauses
    ),
    format('Wrote converted file: ~w~n', [OutputFile]).


is_clause((:- _)) :- !, fail.
is_clause((?- _)) :- !, fail.
is_clause(_) :- !.


read_file_to_clauses(File, Clauses) :-
    setup_call_cleanup(
        open(File, read, Stream),
        ( set_stream(Stream, encoding(utf8)),
          read_clauses(Stream, [], Clauses)
        ),
        close(Stream)
    ).
    
read_clauses(Stream, Acc, Clauses) :-
    read_term(Stream, Term, [variable_names(VNs),module(starlog_to_prolog)]),
	    numbervars(Term, 0, _),
    (   Term == end_of_file
    ->  reverse(Acc, Clauses)
    ;   read_clauses(Stream, [(Term,VNs)|Acc], Clauses)
    ).


% --- Enhanced Converter: Starlog clause to Prolog clause with decompression ---
% User-defined predicates keep their original head structure
% Only built-in predicates in the body are converted from Starlog "is" syntax

/*
starlog_to_pl_with_decompression(((Head :- Body),VNs), 
((Head :- PBody),VNs)
) :- 
    %!, 
    %round_to_square
    ([Body]=Body1),
    decompress_goal(Body1, VNs,PBody),!.%term_to_atom(PBody,PBody1).
    */
starlog_to_pl_with_decompression(((Head :- Body),VNs), 
((Head :- PBody1), VNs)
) :- 
    %!, 
    %trace,
    %round_to_square
    ([Body]=Body1),
    decompress_goal(Body1, VNs,PBody),
    PBody=[PBody1,_],!.
    %append(PBody1,[_],PBody),
    %round_to_square(PBody2,PBody1),!.%term_to_atom(PBody,PBody1).
starlog_to_pl_with_decompression(Fact, Fact).


% Decompress a single goal, potentially expanding nested expressions
decompress_goal([], _VNs, _) :- !.
decompress_goal([Goal|Goals], VNs, [Goal2|Goals1]) :-
%trace,
    % Handle conjunctions by processing each goal separately
    (  
        decompress_goal(Goal, VNs, Goal2)
    ->
        %rebuild_conjunction([Goals1a, Goals1b], Goals1),
        format('  Decompressed nested goal: ~w~n', [Goal2])
    ; pretty_goal_list(Goal, VNs, _Goals, Goal2) ->
        format('  Decompressed nested goal: ~w~n', [Goal2])
    ; % If pretty_goal_list fails, convert using starlog_goal_to_pl
        starlog_goal_to_pl(Goal, Goal2)
    ),
    decompress_goal(Goals, VNs, Goals1).


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
% Conditionals and control structures
is_standard_prolog_goal((_ -> _)).
is_standard_prolog_goal((_ ; _)).
is_standard_prolog_goal((_ -> _ ; _)).
% Comparison operators
is_standard_prolog_goal(_ > _).
is_standard_prolog_goal(_ < _).
is_standard_prolog_goal(_ >= _).
is_standard_prolog_goal(_ =< _).
is_standard_prolog_goal(_ == _).
is_standard_prolog_goal(_ \== _).
is_standard_prolog_goal(_ =:= _).
is_standard_prolog_goal(_ =\= _).


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
% Only treat as liftable if it's exactly a known expression with the right arity.
is_truly_nested_expression(Expr) :-
    compound(Expr),
    functor(Expr, F, A),
    F \= '$VAR',
    \+ is_list_structure(Expr),
    liftable_expr_functor(F, A),
    !.


liftable_expr_functor(':', 2).
liftable_expr_functor('&', 2).
liftable_expr_functor('•', 2).
liftable_expr_functor(sqrt, 1).
liftable_expr_functor(round, 1).
liftable_expr_functor(ceiling, 1).
    
% Check if this is a list structure (internal representation of lists)
% Note: [] handles the list syntax sugar while '[]' handles the atom representation
is_list_structure([]).          % Empty list syntax sugar
is_list_structure([_|_]).       % Non-empty list syntax sugar
is_list_structure('[]').        % Empty list as atom (after read_term processing)
% For compound terms, check functor first to avoid instantiation errors
is_list_structure(Term) :-      
    compound(Term),
    functor(Term, Functor, _),
    (Functor = '.' ; Functor = '[|]').


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


% Decompose a nested expression into a fresh variable and prerequisite goals
decompose_nested_expression(Expr, Var, Goals) :-
    create_goal_for_expr(Expr, Var, Goal),
    extract_subexpressions(Expr, SubGoals),
    append(SubGoals, [Goal], Goals).
        
% Create a goal that computes the given expression
create_goal_for_expr(Expr, Var, Goal) :-
    compound(Expr),
    Expr =.. [F|Args],
    compile_expr_functor(F, Args, Var, Goal),
    !.
create_goal_for_expr(Expr, Var, (Var = Expr)).


compile_expr_functor(':', [A,B], Var, string_concat(A,B,Var)) :- !.
compile_expr_functor('&', [A,B], Var, append(A,B,Var)) :- !.
compile_expr_functor('•', [A,B], Var, atom_concat(A,B,Var)) :- !.
compile_expr_functor(sqrt, [A], Var, sqrt(A,Var)) :- !.
compile_expr_functor(round, [A], Var, round(A,Var)) :- !.
compile_expr_functor(ceiling, [A], Var, ceiling(A,Var)) :- !.


% Fallback: if it’s some other functor, don’t “invent” Functor(...,Var).
compile_expr_functor(_F, _Args, Var, (Var = _Expr)) :- fail.


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
        open(File, write, Stream, [encoding(utf8)]),
        ( write_file_header(Stream),
          write_clauses(Stream, Clauses),
          flush_output(Stream)
        ),
        close(Stream)
    ).
    
write_file_header(Stream) :-
    format(Stream, '% Generated by Starlog to Prolog Converter~n', []),
    format(Stream, '% Date/Time: 2025-07-02 00:25:18 UTC~n', []),
    format(Stream, '% User: luciangreenPlease~n', []),
    format(Stream, '% Decompression applied where possible~n~n', []).


write_clauses(_, []).
write_clauses(Stream, [Clause|Clauses]) :-
%trace,
    ( Clause = ((Head :- true
    ),VNs) ->
    %with_output_to(atom(S),write_term(Head, [variable_names(VNs),quoted(true), numbervars(true)]))),
    %write_term(Stream, S, []).
    %/* 
    
    %with_output_to(atom(S2),write_term(Head, [variable_names(VNs),quoted(true), numbervars(true)])), 
        %with_output_to(string(S),write_term(S2, [variable_names(VNs),quoted(true), numbervars(true)])), 
        %term_to_atom(S1,S),
        term_to_atom(Head,S),
        term_to_atom_protocol(Head,S1),
        write(Stream,S1),
        write(Stream, '.\n')
    ; Clause = ((Head :- Body),VNs) ->
    %with_output_to(atom(S2),write_term((Head :- Body), [variable_names(VNs),quoted(true), numbervars(true)])), 
        %with_output_to(string(S),write_term(S2, [variable_names(VNs),quoted(true), numbervars(true)])),
        %term_to_atom(S1,S),
        term_to_atom((Head :- Body),S),
        term_to_atom_protocol((Head :- Body),S1),
        write(Stream,S1),

write(Stream, '.\n')
    ;   Clause = (FactOrRule, _VNs) ->
        % Handle facts and other clauses - extract just the clause part
        term_to_atom(FactOrRule,S),
        term_to_atom_protocol(FactOrRule,S1),
        write(Stream,S1),
     	write(Stream, '.\n')
    ;   % Fallback for clauses without VNs (shouldn't happen)
        term_to_atom(Clause,S),
        term_to_atom_protocol(Clause,S1),
        write(Stream,S1),
     	write(Stream, '.\n')
    ),
    write_clauses(Stream, Clauses).
%*/



% decompress.pl
% Self-contained Starlog expression decompressor + pretty-printer
% Preserves original variable names captured by read_term/3 (variable_names/1).


% ----------------------------
% Public helpers (use these)
% ----------------------------


% Pretty-print from a string that contains a single term ending with '.'
% Example:
% ?- pretty_goal_list_from_atom("E is •(A:(B:(D•F)), C).", Goals).
pretty_goal_list(InputTerm, _VNs, Goals, Goals1) :-
%trace,
    once(decompress_nested_goal(InputTerm, Goals)),
    % Simply return the Goals as a proper term
    % The numbervars(true) option in write_term will handle $VAR(N) -> A, B, C conversion
    square_to_round(Goals, Goals1).
    % Rebuild conjunction from list of goals
    %rebuild_conjunction(Goals2, Goals1).
        
% Pretty-print from a stream (file, etc.)
% Example:
% ?- open('x.pl', read, S), pretty_goal_list_from_stream(S, Goals), close(S).
pretty_goal_list_from_stream(Stream, Goals) :-
    read_term(Stream, Term, [variable_names(VNs)]),
    pretty_goal_list(Term, VNs, Goals).


% Core pretty printer: requires variable_names captured at read-time.
pretty_goal_list(InputTerm, VNs, Goals1) :-
    once(decompress_nested_goal(InputTerm, Goals)),


    term_variables(InputTerm, InVars),
    term_variables(Goals, GoalVars),
    temps_only(GoalVars, InVars, Temps),


    extend_varnames(VNs, Temps, VNs2),
%trace,
    with_output_to(compound(Goals1), write_term(Goals, [variable_names(VNs2)])),
    nl.


% Make a pretty-named COPY of Goals using VNs2 (doesn't change Goals).
pretty_copy_with_varnames(Goals, VNs2, Goals1) :-
    copy_term(Goals, Goals1, GoalsGVars, Goals1GVars),
    % GoalsGVars is the list of vars in Goals (in a stable order),
    % Goals1GVars is the corresponding list of vars in Goals1.
    % Now apply the name->var mapping to the *copy*.
    apply_varnames_to_copy(VNs2, GoalsGVars, Goals1GVars).


apply_varnames_to_copy([], _, _).
apply_varnames_to_copy([_Name=V|Rest], FromVars, ToVars) :-
    % find V by identity in FromVars, unify corresponding var in ToVars
    nth0(I, FromVars, V0), V0 == V, !,
    nth0(I, ToVars, V1),
    V = V0, V1 = V1,  % no-op; we just needed I
    apply_varnames_to_copy(Rest, FromVars, ToVars).
    
% ----------------------------
% Temp detection (identity-safe)
% ----------------------------


temps_only(GoalVars, InVars, Temps) :-
    exclude(is_in_by_identity(InVars), GoalVars, Temps).


is_in_by_identity(InVars, V) :-
    member(V2, InVars),
    V == V2.


% ----------------------------
% Naming: preserve input names, add temp names G,H,I... skipping clashes
% ----------------------------


extend_varnames(VNs, Temps, VNsOut) :-
    used_names(VNs, Used),
    name_temps(Temps, Used, TempPairs),
    append(VNs, TempPairs, VNsOut).


used_names([], []).
used_names([Name=_|Rest], [Name|More]) :-
    used_names(Rest, More).


name_temps([], _Used, []).
name_temps([V|Vs], Used0, [Name=V|Rest]) :-
    next_free_letter(0'G, Used0, Name),
    name_temps(Vs, [Name|Used0], Rest).


next_free_letter(Code0, Used, Name) :-
    char_code(Cand, Code0),
    (   memberchk(Cand, Used)
    ->  Code1 is Code0 + 1,
        next_free_letter(Code1, Used, Name)
    ;   Name = Cand
    ).
    
% ----------------------------
% Decompression: D is Expr  -> list of Prolog goals
% ----------------------------


decompress_nested_goal((Out is Expr), Goals) :-
%trace,
    compile_expr(Expr, Out, Goals).


% compile_expr(+Expr, +Out, -Goals)
% Goals compute Expr into Out.


% • /2  → atom_concat
compile_expr(Expr, Out, Goals) :-
    ( Expr = (L • R) ; Expr =.. ['•', L, R] ),
    !,
    compile_value(L, LV, GL),
    compile_value(R, RV, GR),
    append(GL, GR, G0),
    append(G0, [atom_concat(LV, RV, Out)], Goals).


% : /2  → string_concat
compile_expr(Expr, Out, Goals) :-
    ( Expr = (L : R) ; Expr =.. [':', L, R] ),
    !,
    compile_value(L, LV, GL),
    compile_value(R, RV, GR),
    append(GL, GR, G0),
    append(G0, [string_concat(LV, RV, Out)], Goals).

compile_expr(Expr, Out, Goals) :-
    ( Expr = (L & R) ; Expr =.. ['&', L, R] ),
    !,
    compile_value(L, LV, GL),
    compile_value(R, RV, GR),
    append(GL, GR, G0),
    append(G0, [append(LV, RV, Out)], Goals).


% Built-in value functors → Functor(..., Out)
/*
compile_expr(Expr, Out, Goals) :-
    compound(Expr),
    Expr =.. [F|Args],
    is_value_builtin(F, Arity),
    length(Args, Arity),
    !,
    compile_values(Args, Vals, Gs),
    append(Gs, [Goal], Goals),
    append(Vals, [Out], ValsWithOut),
    Goal =.. [F|ValsWithOut].
*/

compile_expr(Expr, Out, Goals) :-
    %( Expr = (L : R) ; Expr =.. [':', L, R] ),
    %trace,
    Expr =.. [F|Args],
    !,
    %compile_value(L, LV, GL),
    %compile_value(R, RV, GR),
    compile_values(Args, Vals, Gs),
    %append(GL, GR, G0),
    append(Gs, [Goal], Goals),
    append(Vals, [Out], ValsWithOut),
    Goal =.. [F|ValsWithOut],
    append(G0, [Goal], Goals).

/*
x:
compile_expr(Expr, Out, Goals) :-
trace,
    compound(Expr),
    Expr =.. [F|Args],
    is_value_builtin(F, Arity),
    length(Args, Arity),
    !,
    compile_values(Args, Vals, Gs),
    append(Gs, [Goal], Goals),
    append(Vals, [Out], ValsWithOut),
    Goal =.. [F|ValsWithOut].
*/

% Arithmetic expressions → Out is Expr
compile_expr(Expr, Out, [Out is Expr]) :-
    is_arith_expr(Expr),
    !.


% Leaf values (variables/atoms/strings/etc.)
compile_expr(Expr, Out, []) :-
    Out = Expr.


% compile_value(+Expr, -Value, -Goals)
% Ensures Expr is available as a "value" for concat/builtins.
% - If Expr is an expression-form (: or •) -> compute into fresh temp.
% - If Expr is arithmetic -> compute into fresh temp via is/2.
% - Else treat as data/leaf.
compile_value(Expr, Value, Goals) :-
    ( is_expr_form(Expr) ->
        fresh_var(Value),
        compile_expr(Expr, Value, Goals)
    ; is_arith_expr(Expr) ->
        fresh_var(Value),
        Goals = [Value is Expr]
    ;   Value = Expr,
        Goals = []
    ).


compile_values([], [], []).
compile_values([A|As], [V|Vs], Goals) :-
    compile_value(A, V, GA),
    compile_values(As, Vs, GB),
    append(GA, GB, Goals).


fresh_var(V) :- V = _.


% Expression operators
is_expr_form(Expr) :-
    compound(Expr),
    functor(Expr, F, A),
    (memberchk(F/A, [(':')/2, ('•')/2, ('&')/2])->true;
    is_value_builtin(F,A)).


% Built-ins that return a value in the last argument (same idea as your prior list)
is_value_builtin(sqrt, 1).
is_value_builtin(round, 1).
is_value_builtin(ceiling, 1).
is_value_builtin(string_length, 1).
is_value_builtin(atom_length, 1).
is_value_builtin(string_chars, 1).
is_value_builtin(atom_chars, 1).
is_value_builtin(string_to_number, 1).
is_value_builtin(random, 1).
is_value_builtin(sort, 1).
is_value_builtin(reverse, 1).
is_value_builtin(intersection, 2).
is_value_builtin(head, 1).
is_value_builtin(tail, 1).
is_value_builtin(delete, 2).
is_value_builtin(wrap, 1).
is_value_builtin(unwrap, 1).
is_value_builtin(maplist, 2).


% Arithmetic
is_arith_expr(Expr) :-
    compound(Expr),
    functor(Expr, F, 2),
    memberchk(F, [+, -, *, /, //, mod, **]).



% replace_var_terms(+TermIn, -TermOut)
replace_var_terms(TermIn, TermOut) :-
    % Check if TermIn is a term like $VAR(N)
    TermIn = '$VAR'(N),
    integer(N),
    % Map N to a capital letter (0 -> 'A', 1 -> 'B', etc.)
    Char is 65 + N, % ASCII value for 'A' is 65
    atom_codes(TermOut, [Char]), !. % Convert char code to an atom
    
replace_var_terms(TermIn, TermIn) :-
    % If it's a variable, list, or atomic type other than $VAR/1, do nothing.
    (   var(TermIn)
    ;   atomic(TermIn)
    ), !.

replace_var_terms(TermIn, TermOut) :-
    % If it's a compound term (including lists), recurse through its arguments
    TermIn =.. [Functor|ArgsIn],
    maplist(replace_var_terms, ArgsIn, ArgsOut),
    TermOut =.. [Functor|ArgsOut].


% Transform List

:-dynamic new_var_sl2p888/1.
:-dynamic free_vars_sl2p888/1.

% transform_list(+OldList, -NewList).
% Traverses OldList, replacing all atoms starting with a capital letter with unique variables in NewList.
transform_list([], []).
transform_list([H|T], [H_new|T_new]) :-
    transform_term(H, H_new),
    transform_list(T, T_new).

% transform_term(+OldTerm, -NewTerm).
% Transforms an individual term.
% If it's a capitalized atom, a new unique variable is used.
% If it's a compound term, it is traversed recursively.
% Other terms (lowercase atoms, numbers, variables) remain unchanged.
transform_term(Term, Term) :-
    var(Term), !. % Variables are kept as is

get_new_var(N1) :-
	starlog_to_prolog:new_var_sl2p888(N),
	N1 is N+1,
	retractall(new_var_sl2p888(_)),
	assertz(new_var_sl2p888(N1)),!.


transform_term(Term, V_name) :-
    atom(Term),
    atom_chars(Term, [Char1|_]),
    char_type(Char1, upper),
    %trace,
    starlog_to_prolog:free_vars_sl2p888(FV),
    (member([Term,N],FV)->true;
    (%trace,
    get_new_var(N),
    append(FV,[[Term,N]],FV1),
	retractall(free_vars_sl2p888(_)),
	assertz(free_vars_sl2p888(FV1)))),
    atom_concat('A',N,V_name1),
    %trace,
    V_name = _,      
    N1 is N+26,
    numbervars(V_name, N1, _),
    %var_num(V_name,V_name2),
    %write_term(V_name, [numbervars(true)]),
	%with_output_to(atom(V_name),write_term(V_name1, [quoted(false)])),
    !.
    % For a capitalized atom, we generate a new unique variable.
    % In Prolog, simply using an uninstantiated variable acts as a unique one.
    % The binding happens automatically when the NewTerm variable is unified.
    %NewTerm = _. 

transform_term(Term, Term) :-
    atom(Term), !. % Lowercase atoms are kept as is

transform_term(Term, Term) :-
    number(Term), !. % Numbers are kept as is

transform_term(Term, NewTerm) :-
    compound(Term),
    Term =.. [Functor|Args],
    transform_list(Args, NewArgs), % Recursively transform arguments
    NewTerm =.. [Functor|NewArgs],
    !.

transform_term(Term, Term) :- Term = [], !.

% Helper to check character type (SWI-Prolog specific)
% Assumes default character encoding
%:- use_module(library(ctype)).

open_file_s_s(File1,String) :-

   atom_string(File2, File1),
   phrase_from_file(string(Codes), File2),
   string_codes(String, Codes),!.

term_to_atom_protocol(Term,Atom2) :-
	protocol('tmp32478.txt'),
	write_term(Term, [quoted(true), numbervars(true)]),
	nl,
	noprotocol,
	open_file_s_s("tmp32478.txt",String),
	atom_string(String,Atom1),
	atom_concat(Atom2,'\n',Atom1),
	rm("tmp32478.txt").

conjunction_list(C, [C])   :- not(C = (_,_)).
conjunction_list(C, [P|R]) :- C = (P,Q), conjunction_list(Q, R).
 
square_to_round(A,B) :-
 conjunction_list(B,A).

string(String) --> list(String).

list([]) --> [].
list([L|Ls]) --> [L], list(Ls).
