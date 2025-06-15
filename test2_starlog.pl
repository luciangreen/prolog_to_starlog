make_file_path(V0,V1,V2,V3):-string_constant(V4),V5 is •(V0,V4),V6 is •(V5,V1),V7 is •(V6,V4),V3 is •(V7,V2).
process_lists(V0,V1,V2,V3,V4):-V5 is &(V0,V1),V6 is V2:V3,V4 is &(V5,[V6]).
employee(john,developer,2020).
employee(jane,manager,2018).
employee(bob,tester,2021).
senior_employee(V0,V1,V2):-employee(V0,V1,V2),V2<2020.
format_employee(V0,V1,V2,V3):-string_constant(V4),V5 is V0:V4,V6 is V5:V1,string_constant(V7),V8 is V6:V7,V9 is V8:V2,string_constant(V10),V3 is V9:V10.
filter_recent_employees([],V0,[]).
filter_recent_employees([employee(V0,V1,V2)|V3],V4,[V0|V5]):-V2>=V4,filter_recent_employees(V3,V4,V5).
filter_recent_employees([V0|V1],V2,V3):-filter_recent_employees(V1,V2,V3).
