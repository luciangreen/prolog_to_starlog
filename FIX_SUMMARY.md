# Fix for Compound Term Handling in Starlog Decompression

## Problem Statement

When converting Starlog expressions to Prolog, compound terms containing the `:`, `•`, or `&` operators were incorrectly being expanded even when they appeared as arguments to functions. This caused errors like:

1. `?- A is [a:a].` → ERROR: Type error: `character' expected, found `a:a' (a compound)
2. `?- A is wrap(a:a).` → ERROR: Unknown procedure: wrap/2
3. `?- A is string_number(1:1).` → ERROR: Arithmetic: `(:)/2' is not a function

## Root Cause

In `starlog_to_prolog_cli.pl`, the `compile_value/3` predicate was using `is_expr_form/1` to determine if an expression should be decomposed. This predicate treated `:`, `•`, and `&` operators as ALWAYS needing expansion, even when they appeared as arguments to builtin functions.

For example:
- Input: `A is wrap(a:a)`
- Buggy behavior: `a:a` was decomposed into `string_concat(a, a, _G), wrap(_G, A)`
- This caused: `wrap/2` error because `wrap` expects 1 argument, not 2

## Solution

Modified `compile_value/3` in `starlog_to_prolog_cli.pl` to only expand `:`, `•`, and `&` operators when they are builtin function calls, NOT when they appear as compound term arguments.

### Key Changes

1. **New predicate `is_value_builtin_expr/1`**: 
   - Checks if an expression is a builtin function
   - Explicitly excludes `:`, `•`, and `&` operators from expansion when nested
   
2. **Updated `compile_value/3`**:
   - Changed from using `is_expr_form(Expr)` to `is_value_builtin_expr(Expr)`
   - Now treats compound terms like `a:a` as data when they appear as arguments
   - Still expands top-level operator expressions correctly

### Code Changes

```prolog
% OLD CODE (buggy):
compile_value(Expr, Value, Goals) :-
    ( is_expr_form(Expr) ->  % This expanded :, •, & even when nested
        fresh_var(Value),
        compile_expr(Expr, Value, Goals)
    ; ...
    ).

is_expr_form(Expr) :-
    compound(Expr),
    functor(Expr, F, A),
    (memberchk(F/A, [(':')/2, ('•')/2, ('&')/2])->true;  % WRONG!
    is_value_builtin(F,A)).

% NEW CODE (fixed):
compile_value(Expr, Value, Goals) :-
    ( is_value_builtin_expr(Expr) ->  % Only expand builtin functions
        fresh_var(Value),
        compile_expr(Expr, Value, Goals)
    ; ...
    ).

is_value_builtin_expr(Expr) :-
    compound(Expr),
    functor(Expr, F, A),
    F \= ':',    % Don't expand these when nested
    F \= '•',
    F \= '&',
    is_value_builtin(F, A).
```

## Test Results

All test cases pass:

### Test 1: wrap(a:a)
- Input Starlog: `A is wrap(a:a)`
- Output Prolog: `wrap(a:a, A)` ✓
- Compound term `a:a` is preserved

### Test 2: string_to_number(1:1)
- Input Starlog: `A is string_to_number(1:1)`
- Output Prolog: `string_to_number(1:1, A)` ✓
- Compound term `1:1` is preserved

### Test 3: Top-level operators still work
- Input Starlog: `C is (a : b)`
- Output Prolog: `string_concat(a, b, C)` ✓
- Top-level `:` is correctly expanded

### Test 4: List with compound terms
- Input Starlog: `A is [[a:a]]`
- Output Prolog: `A = [[a:a]]` ✓
- Compound terms in lists are preserved

## Files Modified

- `starlog_to_prolog_cli.pl` - Main fix in `compile_value/3` and new `is_value_builtin_expr/1`

## Test Files Created

- `test_compound_fix.pl` - Unit tests for compile_expr and compile_value
- `test_decompression.pl` - Tests for the decompression logic
- `test_full_conversion.pl` - End-to-end conversion tests
- `test_execution.pl` - Execution tests for converted code
- `test_direct_conversion.pl` - Direct conversion tests
- `test_problem_statement_starlog.pl` - Test cases from problem statement

All tests pass successfully.

## Impact

This fix ensures that:
1. Compound terms with `:`, `•`, or `&` operators are correctly preserved when used as arguments
2. Top-level operator expressions are still correctly expanded
3. No breaking changes to existing functionality
4. The errors described in the problem statement are resolved
