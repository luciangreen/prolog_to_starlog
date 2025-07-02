% Test8: Quoted atoms and strings
% Author: luciangreenGo
% Date: 2025-07-02 15:24:50 UTC

% Quoted atoms and strings
message('Hello world').
message("String literal").

greeting(Name, Greeting) :-
    string_concat("Hello, ", Name, Greeting).

special_chars('!@#$%^&*()_+').
containing_quotes('This contains "quotes"').
