% Starlog String Operations
% Author: luciangreenPlease
% Date: 2025-07-02 16:23:54 UTC

% String concatenation
string_test(C) :- 
    string_concat(A, B, C).

% Full name generator
full_name(First, Last, Full) :- 
    string_concat(First, " ", WithSpace),
    string_concat(WithSpace, Last, Full).

% Multiple concatenation
greeting(Name, Greeting) :- 
    string_concat("Hello, ", Name, Temp1),
    string_concat(Temp1, "!", Greeting).

