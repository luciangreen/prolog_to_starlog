% Starlog String Operations
% Author: luciangreenPlease
% Date: 2025-07-02 16:23:54 UTC

% String concatenation
string_test(C) :- C is (A : B).

% Full name generator
full_name(First, Last, Full) :- 
    WithSpace is (First : " "),
    Full is (WithSpace : Last).
    
% Multiple concatenation
greeting(Name, Greeting) :- 
    Greeting is ("Hello, " : Name : "!").
