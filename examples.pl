% Examples of Prolog to Starlog conversion:

% Example 1: Basic built-in predicate
% Prolog:
string_length("hello", Length).
% Starlog:
Length is string_length("hello").

% Example 2: String concatenation
% Prolog:
string_concat("Hello, ", "World!", Greeting).
% Starlog:
Greeting is "Hello, " : "World!".

% Example 3: List append
% Prolog:
append([1, 2], [3, 4], Result).
% Starlog:
Result is [1, 2] & [3, 4].

% Example 4: Atom concatenation
% Prolog:
atom_concat(first, last, FullName).
% Starlog:
FullName is first • last.

% Example 5: Multiple operations in a clause
% Prolog:
make_greeting(Name, Greeting) :-
    string_chars(Name, Chars),
    atom_chars(Atom, Chars),
    atom_concat("Hello, ", Atom, TempGreeting),
    atom_concat(TempGreeting, "!", Greeting).

% Starlog:
make_greeting(Name, Greeting) :-
    Chars is string_chars(Name),
    Atom is atom_chars(Chars),
    TempGreeting is "Hello, " • Atom,
    Greeting is TempGreeting • "!".

% Example 6: Nested operations (Starlog to Prolog only)
% Starlog:
Result is atom_length(string_chars(Name))
% Prolog:
string_chars(Name, Temp1),
atom_length(Temp1, Result).

% Example 7: Built-in predicate with multiple arguments
% Prolog:
sub_string("Hello World", 0, 5, _, Result).
% Starlog:
Result is sub_string("Hello World", 0, 5, _).

% Example 8: Complex clause with multiple operations
% Prolog:
process_text(Input, Output) :-
    string_chars(Input, Chars),
    reverse(Chars, RevChars),
    string_chars(Reversed, RevChars),
    string_concat(Reversed, "!", Output).

% Starlog:
process_text(Input, Output) :-
    Chars is string_chars(Input),
    RevChars = reverse(Chars),
    Reversed is string_chars(RevChars),
    Output is Reversed : "!".

% Example 9: Using number conversion
% Prolog:
number_string(42, String).
% Starlog:
String is number_string(42).

% Example 10: Multiple built-ins in sequence
% Prolog:
format_number(Num, Result) :-
    number_string(Num, String),
    string_concat("Number: ", String, TempStr),
    string_length(TempStr, Len),
    number_string(Len, LenStr),
    string_concat(TempStr, " (length: ", Temp2),
    string_concat(Temp2, LenStr, Temp3),
    string_concat(Temp3, ")", Result).

% Starlog:
format_number(Num, Result) :-
    String is number_string(Num),
    TempStr is "Number: " : String,
    Len is string_length(TempStr),
    LenStr is number_string(Len),
    Temp2 is TempStr : " (length: ",
    Temp3 is Temp2 : LenStr,
    Result is Temp3 : ")".	