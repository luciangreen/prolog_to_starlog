# Starlog Converter Documentation

**Date**: 2025-07-02 16:23:54 UTC  
**Author**: luciangreenPlease

## Overview

The Starlog Converter is a bidirectional translation tool that converts between standard Prolog syntax and Starlog syntax. Starlog is a variant of Prolog that uses a more functional notation, particularly for built-in predicates where the output parameter is represented using an "is" operator rather than as the last argument of a predicate.

## What is Starlog?

Starlog is a Prolog variant that uses the notation `Result is function(Args)` instead of Prolog's `function(Args, Result)`. This makes code more readable when working with transformations and operations that produce a result, as the output variable appears first in the expression rather than at the end of a parameter list.

## Key Conversion Pattern

The primary conversion pattern:

**Prolog**: `predicate(input1, input2, ..., output)`  
**Starlog**: `output is predicate(input1, input2, ...)`

Special operators are used for common operations:
- String concatenation: `C is (A : B)` instead of `string_concat(A, B, C)`
- List append: `C is (A & B)` instead of `append(A, B, C)`
- Atom concatenation: `C is (A • B)` instead of `atom_concat(A, B, C)`

## Complete Built-in Predicate Conversion Lists

### Prolog to Starlog Conversion

The following built-in Prolog predicates are converted to Starlog format:

#### String and Atom Operations
| Prolog | Starlog |
|--------|---------|
| `string_concat(A, B, C)` | `C is (A : B)` |
| `atom_concat(A, B, C)` | `C is (A • B)` |
| `string_length(A, B)` | `B is string_length(A)` |
| `number_string(A, B)` | `B is number_string(A)` |
| `atom_length(A, B)` | `B is atom_length(A)` |
| `sub_string(A, B, C, D, E)` | `E is sub_string(A, B, C, D)` |
| `string_chars(A, B)` | `B is string_chars(A)` |
| `atom_chars(A, B)` | `B is atom_chars(A)` |
| `atom_string(A, B)` | `B is atom_string(A)` |
| `atom_number(A, B)` | `B is atom_number(A)` |
| `char_code(A, B)` | `B is char_code(A)` |
| `string_upper(A, B)` | `B is string_upper(A)` |
| `string_lower(A, B)` | `B is string_lower(A)` |
| `atom_codes(A, B)` | `B is atom_codes(A)` |
| `string_codes(A, B)` | `B is string_codes(A)` |
| `term_string(A, B)` | `B is term_string(A)` |
| `term_to_atom(A, B)` | `B is term_to_atom(A)` |
| `downcase_atom(A, B)` | `B is downcase_atom(A)` |
| `upcase_atom(A, B)` | `B is upcase_atom(A)` |

#### List Operations
| Prolog | Starlog |
|--------|---------|
| `append(A, B, C)` | `C is (A & B)` |
| `length(A, B)` | `B is length_1(A)` |
| `member(A, B)` | `B is member_1(A)` |
| `reverse(A, B)` | `B is reverse(A)` |
| `head(A, B)` | `B is head(A)` |
| `tail(A, B)` | `B is tail(A)` |
| `delete(A, B, C)` | `C is delete(A, B)` |
| `wrap(A, B)` | `B is wrap(A)` |
| `unwrap(A, B)` | `B is unwrap(A)` |
| `maplist(A, B, C)` | `C is maplist(A, B)` |
| `sort(A, B)` | `B is sort(A)` |
| `msort(A, B)` | `B is msort(A)` |
| `keysort(A, B)` | `B is keysort(A)` |
| `intersection(A, B, C)` | `C is intersection(A, B)` |
| `union(A, B, C)` | `C is union(A, B)` |
| `flatten(A, B)` | `B is flatten(A)` |
| `nth0(A, B, C)` | `C is nth0(A, B)` |
| `nth1(A, B, C)` | `C is nth1(A, B)` |
| `last(A, B)` | `B is last(A)` |
| `min_list(A, B)` | `B is min_list(A)` |
| `max_list(A, B)` | `B is max_list(A)` |
| `sum_list(A, B)` | `B is sum_list(A)` |
| `subtract(A, B, C)` | `C is subtract(A, B)` |
| `select(A, B, C)` | `C is select(A, B)` |
| `permutation(A, B)` | `B is permutation(A)` |

#### Math Operations
| Prolog | Starlog |
|--------|---------|
| `is(A, B)` | `A is B` |
| `=(A, B)` | `A = B` |
| `string_to_number(A, B)` | `B is string_to_number(A)` |
| `random(A, B)` | `B is random(A)` |
| `ceiling(A, B)` | `B is ceiling(A)` |
| `sqrt(A, B)` | `B is sqrt(A)` |
| `round(A, B)` | `B is round(A)` |
| `floor(A, B)` | `B is floor(A)` |
| `truncate(A, B)` | `B is truncate(A)` |
| `abs(A, B)` | `B is abs(A)` |
| `sign(A, B)` | `B is sign(A)` |
| `sin(A, B)` | `B is sin(A)` |
| `cos(A, B)` | `B is cos(A)` |
| `tan(A, B)` | `B is tan(A)` |
| `asin(A, B)` | `B is asin(A)` |
| `acos(A, B)` | `B is acos(A)` |
| `atan(A, B)` | `B is atan(A)` |
| `log(A, B)` | `B is log(A)` |
| `log10(A, B)` | `B is log10(A)` |
| `exp(A, B)` | `B is exp(A)` |

#### Other Operations
| Prolog | Starlog |
|--------|---------|
| `date(A)` | `A is date` |
| `findall(A, B, C)` | `C is findall(A, B)` |
| `string_from_file(A, B)` | `B is string_from_file(A)` |
| `read_string(A, B, C, D, E)` | `E is read_string(A, B, C, D)` |
| `term_variables(A, B)` | `B is term_variables(A)` |
| `current_output(A, B)` | `B is current_output(A)` |
| `current_input(A, B)` | `B is current_input(A)` |
| `file_base_name(A, B)` | `B is file_base_name(A)` |
| `file_name_extension(A, B, C)` | `C is file_name_extension(A, B)` |
| `directory_files(A, B)` | `B is directory_files(A)` |
| `working_directory(A, B)` | `B is working_directory(A)` |
| `atomic_list_concat(A, B)` | `B is atomic_list_concat(A)` |
| `atomic_list_concat(A, B, C)` | `C is atomic_list_concat(A, B)` |
| `sub_atom(A, B, C, D, E)` | `E is sub_atom(A, B, C, D)` |
| `format_time(A, B, C, D)` | `D is format_time(A, B, C)` |
| `split_string(A, B, C, D)` | `D is split_string(A, B, C)` |
| `get_time(A)` | `A is get_time` |
| `term_hash(A, B)` | `B is term_hash(A)` |

### Starlog to Prolog Conversion

The reverse conversion transforms Starlog code back to standard Prolog:

#### String and Atom Operations
| Starlog | Prolog |
|---------|--------|
| `C is (A : B)` | `string_concat(A, B, C)` |
| `C is (A • B)` | `atom_concat(A, B, C)` |
| `B is string_length(A)` | `string_length(A, B)` |
| `B is number_string(A)` | `number_string(A, B)` |
| `B is atom_length(A)` | `atom_length(A, B)` |
| `E is sub_string(A, B, C, D)` | `sub_string(A, B, C, D, E)` |
| `B is string_chars(A)` | `string_chars(A, B)` |
| `B is atom_chars(A)` | `atom_chars(A, B)` |
| `B is atom_string(A)` | `atom_string(A, B)` |
| `B is atom_number(A)` | `atom_number(A, B)` |
| `B is char_code(A)` | `char_code(A, B)` |
| `B is string_upper(A)` | `string_upper(A, B)` |
| `B is string_lower(A)` | `string_lower(A, B)` |
| `B is atom_codes(A)` | `atom_codes(A, B)` |
| `B is string_codes(A)` | `string_codes(A, B)` |
| `B is term_string(A)` | `term_string(A, B)` |
| `B is term_to_atom(A)` | `term_to_atom(A, B)` |
| `B is downcase_atom(A)` | `downcase_atom(A, B)` |
| `B is upcase_atom(A)` | `upcase_atom(A, B)` |

#### List Operations
| Starlog | Prolog |
|---------|--------|
| `C is (A & B)` | `append(A, B, C)` |
| `B is length_1(A)` | `length(A, B)` |
| `B is member_1(A)` | `member(A, B)` |
| `B is reverse(A)` | `reverse(A, B)` |
| `B is head(A)` | `head(A, B)` |
| `B is tail(A)` | `tail(A, B)` |
| `C is delete(A, B)` | `delete(A, B, C)` |
| `B is wrap(A)` | `wrap(A, B)` |

## Usage

### Converting Starlog to Prolog

```bash
cd starlog_files
swipl -q -g main -s ../starlog_to_prolog.pl 
```

### Converting Prolog to Starlog

```bash
cd starlog_files
swipl -q -g main -s ../prolog_to_starlog.pl
```
