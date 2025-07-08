main:-convert_all_prolog_to_starlog,halt.
convert_all_prolog_to_starlog:-working_directory(A,A),format(~n=== Prolog to Starlog Converter ===~n),format(Date/Time: 2025-07-02 16:23:54 UTC~n),format(User: luciangreenPlease~n),format(Working in directory: ~w~n,[A]),directory_files(.,B),exclude(hidden_or_special,B,C),include(is_prolog_file,C,D),(D=[]->format(No .pl files found for conversion~n);length(D,E),format(Found ~w files to process: ~w~n,[E,D]),process_prolog_files(D)),format(~n=== Prolog to Starlog Conversion Complete ===~n).
hidden_or_special(A):-sub_atom(A,0,1,B,.).
hidden_or_special(..).
hidden_or_special(.).
is_prolog_file(A):-file_name_extension(B,pl,A),\+sub_atom(A,C,D,0,_starlog),\+sub_atom(A,E,F,0,_prolog),A\=var_utils.pl,A\=prolog_to_starlog.pl,A\=starlog_to_prolog.pl.
process_prolog_files([]).
process_prolog_files([A|B]):-format(Processing file: ~w~n,[A]),catch((process_single_file(A),format(Successfully processed ~w~n,[A])),C,format(Error processing ~w: ~w~n,[A,C])),process_prolog_files(B).
process_single_file(A):-format(Reading file: ~w~n,[A]),read_file_to_clauses(A,B),length(B,C),format(Read ~w raw clauses from ~w~n,[C,A]),include(is_clause,B,D),length(D,E),format(Filtered to ~w valid clauses~n,[E]),maplist(pl_to_starlog,D,F),length(F,G),format(Converted ~w clauses to Starlog~n,[G]),maplist(rename_vars_pretty,F,H),file_name_extension(I,pl,A),atom_concat(I,_starlog.pl,J),copy_file_header(A,J),format(Writing to: ~w~n,[J]),write_clauses_to_file(J,H),format(Wrote converted file: ~w~n,[J]).
copy_file_header(A,B):-setup_call_cleanup(open(A,read,C),setup_call_cleanup(open(B,write,D),copy_header_comments(C,D),close(D)),close(C)).
copy_header_comments(A,B):-read_line_to_string(A,C),(C\=end_of_file,sub_atom(C,0,1,D,%)->format(B,~s~n,[C]),copy_header_comments(A,B);true).
is_clause((:-A)):-!,fail.
is_clause((?-A)):-!,fail.
is_clause(A):-!.
read_file_to_clauses(A,B):-setup_call_cleanup(open(A,read,C),read_clauses(C,[],B),close(C)).
read_clauses(A,B,C):-read_term(A,D,[]),(D==end_of_file->reverse(B,C);read_clauses(A,[D|B],C)).
pl_to_starlog((A:-B),(C:-D)):-!,extract_output_var(A,E,C),pl_body_to_starlog(B,E,D).
pl_to_starlog(A,B):-extract_output_var(A,C,B).
extract_output_var(A,B,C):-A=..[D|E],(get_predicate_output_position(D,E,F),F>0->nth1(F,E,B),delete_nth(E,F,G),C=..[D|G];C=A,B=none).
get_predicate_output_position(string_concat,[A,B,C],3):-!.
get_predicate_output_position(atom_concat,[A,B,C],3):-!.
get_predicate_output_position(string_length,[A,B],2):-!.
get_predicate_output_position(number_string,[A,B],2):-!.
get_predicate_output_position(atom_length,[A,B],2):-!.
get_predicate_output_position(sub_string,[A,B,C,D,E],5):-!.
get_predicate_output_position(string_chars,[A,B],2):-!.
get_predicate_output_position(atom_chars,[A,B],2):-!.
get_predicate_output_position(atom_string,[A,B],2):-!.
get_predicate_output_position(atom_number,[A,B],2):-!.
get_predicate_output_position(char_code,[A,B],2):-!.
get_predicate_output_position(string_upper,[A,B],2):-!.
get_predicate_output_position(string_lower,[A,B],2):-!.
get_predicate_output_position(atom_codes,[A,B],2):-!.
get_predicate_output_position(string_codes,[A,B],2):-!.
get_predicate_output_position(term_string,[A,B],2):-!.
get_predicate_output_position(term_to_atom,[A,B],2):-!.
get_predicate_output_position(downcase_atom,[A,B],2):-!.
get_predicate_output_position(upcase_atom,[A,B],2):-!.
get_predicate_output_position(append,[A,B,C],3):-!.
get_predicate_output_position(length,[A,B],2):-!.
get_predicate_output_position(member,[A,B],2):-!.
get_predicate_output_position(reverse,[A,B],2):-!.
get_predicate_output_position(head,[A,B],2):-!.
get_predicate_output_position(tail,[A,B],2):-!.
get_predicate_output_position(delete,[A,B,C],3):-!.
get_predicate_output_position(wrap,[A,B],2):-!.
get_predicate_output_position(unwrap,[A,B],2):-!.
get_predicate_output_position(maplist,[A,B,C],3):-!.
get_predicate_output_position(sort,[A,B],2):-!.
get_predicate_output_position(msort,[A,B],2):-!.
get_predicate_output_position(keysort,[A,B],2):-!.
get_predicate_output_position(intersection,[A,B,C],3):-!.
get_predicate_output_position(union,[A,B,C],3):-!.
get_predicate_output_position(flatten,[A,B],2):-!.
get_predicate_output_position(nth0,[A,B,C],3):-!.
get_predicate_output_position(nth1,[A,B,C],3):-!.
get_predicate_output_position(last,[A,B],2):-!.
get_predicate_output_position(min_list,[A,B],2):-!.
get_predicate_output_position(max_list,[A,B],2):-!.
get_predicate_output_position(sum_list,[A,B],2):-!.
get_predicate_output_position(subtract,[A,B,C],3):-!.
get_predicate_output_position(select,[A,B,C],3):-!.
get_predicate_output_position(permutation,[A,B],2):-!.
get_predicate_output_position(string_to_number,[A,B],2):-!.
get_predicate_output_position(random,[A,B],2):-!.
get_predicate_output_position(ceiling,[A,B],2):-!.
get_predicate_output_position(sqrt,[A,B],2):-!.
get_predicate_output_position(round,[A,B],2):-!.
get_predicate_output_position(floor,[A,B],2):-!.
get_predicate_output_position(truncate,[A,B],2):-!.
get_predicate_output_position(abs,[A,B],2):-!.
get_predicate_output_position(sign,[A,B],2):-!.
get_predicate_output_position(sin,[A,B],2):-!.
get_predicate_output_position(cos,[A,B],2):-!.
get_predicate_output_position(tan,[A,B],2):-!.
get_predicate_output_position(asin,[A,B],2):-!.
get_predicate_output_position(acos,[A,B],2):-!.
get_predicate_output_position(atan,[A,B],2):-!.
get_predicate_output_position(log,[A,B],2):-!.
get_predicate_output_position(log10,[A,B],2):-!.
get_predicate_output_position(exp,[A,B],2):-!.
get_predicate_output_position(date,[A],1):-!.
get_predicate_output_position(findall,[A,B,C],3):-!.
get_predicate_output_position(string_from_file,[A,B],2):-!.
get_predicate_output_position(read_string,[A,B,C,D,E],5):-!.
get_predicate_output_position(term_variables,[A,B],2):-!.
get_predicate_output_position(current_output,[A,B],2):-!.
get_predicate_output_position(current_input,[A,B],2):-!.
get_predicate_output_position(file_base_name,[A,B],2):-!.
get_predicate_output_position(file_name_extension,[A,B,C],3):-!.
get_predicate_output_position(directory_files,[A,B],2):-!.
get_predicate_output_position(working_directory,[A,B],2):-!.
get_predicate_output_position(atomic_list_concat,[A,B],2):-!.
get_predicate_output_position(atomic_list_concat,[A,B,C],3):-!.
get_predicate_output_position(sub_atom,[A,B,C,D,E],5):-!.
get_predicate_output_position(format_time,[A,B,C,D],4):-!.
get_predicate_output_position(split_string,[A,B,C,D],4):-!.
get_predicate_output_position(get_time,[A],1):-!.
get_predicate_output_position(term_hash,[A,B],2):-!.
get_predicate_output_position(A,B,0).
delete_nth(A,B,C):-length(D,B),append(D,[E|F],A),append(D,F,C).
pl_body_to_starlog(A,B,C):-flatten_conjunction(A,D),maplist(pl_goal_to_starlog,D,E),rebuild_conjunction(E,C).
flatten_conjunction((A,B),C):-!,flatten_conjunction(A,D),flatten_conjunction(B,E),append(D,E,C).
flatten_conjunction(A,[A]).
rebuild_conjunction([A],A):-!.
rebuild_conjunction([A|B],(A,C)):-rebuild_conjunction(B,C).
pl_goal_to_starlog((A,B),(C,D)):-!,pl_goal_to_starlog(A,C),pl_goal_to_starlog(B,D).
pl_goal_to_starlog(string_concat(A,B,C),C is A:B):-!.
pl_goal_to_starlog(atom_concat(A,B,C),C is A•B):-!.
pl_goal_to_starlog(string_length(A,B),B is string_length(A)):-!.
pl_goal_to_starlog(number_string(A,B),B is number_string(A)):-!.
pl_goal_to_starlog(atom_length(A,B),B is atom_length(A)):-!.
pl_goal_to_starlog(sub_string(A,B,C,D,E),E is sub_string(A,B,C,D)):-!.
pl_goal_to_starlog(string_chars(A,B),B is string_chars(A)):-!.
pl_goal_to_starlog(atom_chars(A,B),B is atom_chars(A)):-!.
pl_goal_to_starlog(atom_string(A,B),B is atom_string(A)):-!.
pl_goal_to_starlog(atom_number(A,B),B is atom_number(A)):-!.
pl_goal_to_starlog(char_code(A,B),B is char_code(A)):-!.
pl_goal_to_starlog(string_upper(A,B),B is string_upper(A)):-!.
pl_goal_to_starlog(string_lower(A,B),B is string_lower(A)):-!.
pl_goal_to_starlog(atom_codes(A,B),B is atom_codes(A)):-!.
pl_goal_to_starlog(string_codes(A,B),B is string_codes(A)):-!.
pl_goal_to_starlog(term_string(A,B),B is term_string(A)):-!.
pl_goal_to_starlog(term_to_atom(A,B),B is term_to_atom(A)):-!.
pl_goal_to_starlog(downcase_atom(A,B),B is downcase_atom(A)):-!.
pl_goal_to_starlog(upcase_atom(A,B),B is upcase_atom(A)):-!.
pl_goal_to_starlog(append(A,B,C),C is A&B):-!.
pl_goal_to_starlog(length(A,B),B is length_1(A)):-!.
pl_goal_to_starlog(member(A,B),B is member_1(A)):-!.
pl_goal_to_starlog(reverse(A,B),B is reverse(A)):-!.
pl_goal_to_starlog(head(A,B),B is head(A)):-!.
pl_goal_to_starlog(tail(A,B),B is tail(A)):-!.
pl_goal_to_starlog(delete(A,B,C),C is delete(A,B)):-!.
pl_goal_to_starlog(wrap(A,B),B is wrap(A)):-!.
pl_goal_to_starlog(unwrap(A,B),B is unwrap(A)):-!.
pl_goal_to_starlog(maplist(A,B,C),C is maplist(A,B)):-!.
pl_goal_to_starlog(sort(A,B),B is sort(A)):-!.
pl_goal_to_starlog(msort(A,B),B is msort(A)):-!.
pl_goal_to_starlog(keysort(A,B),B is keysort(A)):-!.
pl_goal_to_starlog(intersection(A,B,C),C is intersection(A,B)):-!.
pl_goal_to_starlog(union(A,B,C),C is union(A,B)):-!.
pl_goal_to_starlog(flatten(A,B),B is flatten(A)):-!.
pl_goal_to_starlog(nth0(A,B,C),C is nth0(A,B)):-!.
pl_goal_to_starlog(nth1(A,B,C),C is nth1(A,B)):-!.
pl_goal_to_starlog(last(A,B),B is last(A)):-!.
pl_goal_to_starlog(min_list(A,B),B is min_list(A)):-!.
pl_goal_to_starlog(max_list(A,B),B is max_list(A)):-!.
pl_goal_to_starlog(sum_list(A,B),B is sum_list(A)):-!.
pl_goal_to_starlog(subtract(A,B,C),C is subtract(A,B)):-!.
pl_goal_to_starlog(select(A,B,C),C is select(A,B)):-!.
pl_goal_to_starlog(permutation(A,B),B is permutation(A)):-!.
pl_goal_to_starlog(A is B,A is B):-!.
pl_goal_to_starlog(A=B,A=B):-!.
pl_goal_to_starlog(string_to_number(A,B),B is string_to_number(A)):-!.
pl_goal_to_starlog(random(A,B),B is random(A)):-!.
pl_goal_to_starlog(ceiling(A,B),B is ceiling(A)):-!.
pl_goal_to_starlog(sqrt(A,B),B is sqrt(A)):-!.
pl_goal_to_starlog(round(A,B),B is round(A)):-!.
pl_goal_to_starlog(floor(A,B),B is floor(A)):-!.
pl_goal_to_starlog(truncate(A,B),B is truncate(A)):-!.
pl_goal_to_starlog(abs(A,B),B is abs(A)):-!.
pl_goal_to_starlog(sign(A,B),B is sign(A)):-!.
pl_goal_to_starlog(sin(A,B),B is sin(A)):-!.
pl_goal_to_starlog(cos(A,B),B is cos(A)):-!.
pl_goal_to_starlog(tan(A,B),B is tan(A)):-!.
pl_goal_to_starlog(asin(A,B),B is asin(A)):-!.
pl_goal_to_starlog(acos(A,B),B is acos(A)):-!.
pl_goal_to_starlog(atan(A,B),B is atan(A)):-!.
pl_goal_to_starlog(log(A,B),B is log(A)):-!.
pl_goal_to_starlog(log10(A,B),B is log10(A)):-!.
pl_goal_to_starlog(exp(A,B),B is exp(A)):-!.
pl_goal_to_starlog(date(A),A is date):-!.
pl_goal_to_starlog(findall(A,B,C),C is findall(A,B)):-!.
pl_goal_to_starlog(string_from_file(A,B),B is string_from_file(A)):-!.
pl_goal_to_starlog(read_string(A,B,C,D,E),E is read_string(A,B,C,D)):-!.
pl_goal_to_starlog(term_variables(A,B),B is term_variables(A)):-!.
pl_goal_to_starlog(current_output(A,B),B is current_output(A)):-!.
pl_goal_to_starlog(current_input(A,B),B is current_input(A)):-!.
pl_goal_to_starlog(file_base_name(A,B),B is file_base_name(A)):-!.
pl_goal_to_starlog(file_name_extension(A,B,C),C is file_name_extension(A,B)):-!.
pl_goal_to_starlog(directory_files(A,B),B is directory_files(A)):-!.
pl_goal_to_starlog(working_directory(A,B),B is working_directory(A)):-!.
pl_goal_to_starlog(atomic_list_concat(A,B),B is atomic_list_concat(A)):-!.
pl_goal_to_starlog(atomic_list_concat(A,B,C),C is atomic_list_concat(A,B)):-!.
pl_goal_to_starlog(sub_atom(A,B,C,D,E),E is sub_atom(A,B,C,D)):-!.
pl_goal_to_starlog(format_time(A,B,C,D),D is format_time(A,B,C)):-!.
pl_goal_to_starlog(split_string(A,B,C,D),D is split_string(A,B,C)):-!.
pl_goal_to_starlog(get_time(A),A is get_time):-!.
pl_goal_to_starlog(term_hash(A,B),B is term_hash(A)):-!.
pl_goal_to_starlog(is_space(A),is_space(A)):-!.
pl_goal_to_starlog(true,true):-!.
pl_goal_to_starlog(A,A).
write_clauses_to_file(A,B):-setup_call_cleanup(open(A,append,C),(write_clauses(C,B),flush_output(C)),close(C)).
write_clauses(A,[]).
write_clauses(A,[B|C]):-(B=(D:-true)->write_term(A,D,[quoted(false),numbervars(true)]),write(A,.
);B=(D:-E)->write_term(A,(D:-E),[quoted(false),numbervars(true)]),write(A,.
);write_term(A,B,[quoted(false),numbervars(true)]),write(A,.
)),write_clauses(A,C).
