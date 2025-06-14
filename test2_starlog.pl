make_file_path(A0,A1,A2,A3):-string_constant(A4),A5 is …(A0,A4),A6 is …(A5,A1),A7 is …(A6,A4),A3 is …(A7,A2).
process_lists(A0,A1,A2,A3,A4):-A5 is &(A0,A1),A6 is A2:A3,A4 is &(A5,[A6]).
employee(john,developer,2020).
employee(jane,manager,2018).
employee(bob,tester,2021).
senior_employee(A0,A1,A2):-employee(A0,A1,A2),A2<2020.
format_employee(A0,A1,A2,A3):-string_constant(A4),A5 is A0:A4,A6 is A5:A1,string_constant(A7),A8 is A6:A7,A9 is A8:A2,string_constant(A10),A3 is A9:A10.
filter_recent_employees([],A0,[]).
filter_recent_employees([employee(A0,A1,A2)|A3],A4,[A0|A5]):-A2>=A4,filter_recent_employees(A3,A4,A5).
filter_recent_employees([A0|A1],A2,A3):-filter_recent_employees(A1,A2,A3).
