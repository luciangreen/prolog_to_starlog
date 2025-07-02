% Test10: Complex test cases with multiple features
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% Database
employee(1, 'John', 'Doe', 50000).
employee(2, 'Jane', 'Smith', 60000).
employee(3, 'Bob', 'Johnson', 45000).
employee(4, 'Alice', 'Williams', 70000).

department(1, 'Engineering').
department(2, 'Marketing').
department(3, 'HR').

works_in(1, 1). % John works in Engineering
works_in(2, 2). % Jane works in Marketing
works_in(3, 3). % Bob works in HR
works_in(4, 1). % Alice works in Engineering

% Complex queries
employee_details(ID, FullName, Department, Salary) :-
    employee(ID, FirstName, LastName, Salary),
    works_in(ID, DeptID),
    department(DeptID, Department),
    string_concat(FirstName, " ", WithSpace),
    string_concat(WithSpace, LastName, FullName).

high_salary_employees(Threshold, Employees) :-
    findall(Name, 
           (employee(_, FirstName, LastName, Salary), 
            Salary > Threshold,
            string_concat(FirstName, " ", WithSpace),
            string_concat(WithSpace, LastName, Name)), 
           Employees).

avg_salary_by_dept(DeptID, AvgSalary) :-
    findall(Salary, 
           (works_in(EmpID, DeptID), 
            employee(EmpID, _, _, Salary)), 
           Salaries),
    sum_list(Salaries, Total),
    length(Salaries, Count),
    Count > 0,
    AvgSalary is Total / Count.

% Helper
sum_list([], 0).
sum_list([H|T], Sum) :- sum_list(T, TSum), Sum is H + TSum.
