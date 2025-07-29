## Overview

This project provides two-way translation between a subset of Prolog and Starlog syntax.  
It consists of two main CLI scripts:
- `prolog_to_starlog_cli.pl`
- `starlog_to_prolog_cli.pl`

Each script processes all matching `.pl` files in the current directory, generating output files with appropriate suffixes.

---

## Functional Requirements

### 1. File Detection & Processing
- Must operate in the current working directory.
- For Prolog→Starlog: process all `.pl` files except those ending in `_starlog.pl`, `_cli.pl`, or named `var_utils.pl`.
- For Starlog→Prolog: process all files ending in `_starlog.pl`.
- For each processed file, generate a corresponding output file with `_starlog.pl` or `_prolog.pl` appended to the base name.

### 2. Clause Parsing and Filtering
- Read all clauses (facts and rules) from each source file.
- Exclude directives (`:- ...`) and queries (`?- ...`).

### 3. Variable Renaming
- Rename variables to human-friendly names (`A`, `B`, ..., `Z`, `A1`, ...), using the shared `var_utils.pl`.

### 4. Quoted String and Atom Preservation
- All quoted strings (e.g., `""`, `"foo"`) and atoms (`'bar'`) **must** be preserved and printed quoted in output.
- Use Prolog's `write_term/3` with `[quoted(true), numbervars(true)]` for output.

### 5. Prolog→Starlog Conversion with Compression
- Transform standard Prolog list and string operations to Starlog equivalents:
    - `string_concat(A,B,C)` ⇒ `C is A : B`
    - `append(A,B,C)` ⇒ `C is A & B`
    - `atom_concat(A,B,C)` ⇒ `C is A • B`
    - `string_length(A,B)` ⇒ `B is string_length(A)`
    - `atom_length(A,B)` ⇒ `B is atom_length(A)`
    - etc.
- **Compression into nested predicate calls**: When converting to Starlog, compress predicate calls with a single output into arguments where this output is used:
    - Remove unit production predicates when possible
    - Use nested arguments when an argument is used only once
    - When an argument is used more than once, compute it before the call and use the variable
    - Example: `foo(A,B,C), bar(C,D)` ⇒ `bar(foo(A,B),D)` (if C is used only once)
- Only apply these conversions for built-in predicates with the correct arity and argument types.

### 6. Starlog→Prolog Conversion with Decompression
- Transform Starlog operations back to Prolog only when safe:
    - `A is (B : C)` ⇒ `string_concat(B, C, A)` **only if** `B` and `C` are atoms, strings, or variables
    - Same pattern for `&` (`append`) and `•` (`atom_concat`)
- **Decompression from nested predicate calls**: When converting to Prolog, flatten algorithms with multiple levels of nested calls:
    - Compute arguments before calls in the order that they are needed
    - Extract nested function calls into separate predicates called in sequence
    - Maintain proper variable dependencies and execution order
    - Example: `bar(foo(A,B),D)` ⇒ `foo(A,B,C), bar(C,D)`
- If the Starlog `is`-expression involves compound terms that are not atoms/strings/variables, **do not** convert to a built-in Prolog predicate; pass through unchanged.

### 7. Parallel Conversion Architecture
- Implement a parallel Starlog to Prolog converter that mirrors the Prolog to Starlog converter
- Both converters should handle the same set of built-in predicates and transformations
- Ensure bidirectional consistency: Prolog→Starlog→Prolog should preserve semantics
- Handle edge cases uniformly across both conversion directions

### 8. Robustness
- Scripts must never fail or halt on unhandled clause patterns.  
- All input clauses must appear in the output (possibly unchanged if not recognized for translation).
- If a syntax or conversion pattern is not recognized, output the clause as-is.

### 9. Diagnostics
- Print diagnostic messages to the console indicating:
    - Working directory
    - Files being processed
    - Number and content of clauses read and written
    - Compression/decompression operations performed

### 10. Output
- For each input file processed, always write an output file, even if empty.
- Output must be well-formed Prolog syntax, suitable for reloading with SWI-Prolog.

---

## Non-Functional Requirements

- Code must be compatible with SWI-Prolog 8.x or above.
- All file and stream handling must use `setup_call_cleanup/3`.
- Code must be modular: variable renaming logic must be in a separate `var_utils.pl`.
- Code must be self-documenting, with clear section headers and comments.
- Scripts must be robust to unusual input, such as empty files or files containing only comments/directives.
- Compression and decompression algorithms must preserve program semantics.