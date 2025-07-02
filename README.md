# Starlog-Prolog Converter Documentation

**Date**: 2025-07-02 15:24:50 UTC  
**Author**: luciangreenGo

## Overview

The Starlog-Prolog Converter is a bidirectional translation tool that converts between standard Prolog syntax and Starlog syntax. Starlog is a variant of Prolog that uses a more functional notation for certain predicates, especially built-in functions like string concatenation and list operations.

## Key Features

### 1. Bidirectional Conversion

- **Prolog → Starlog**: Converts standard Prolog code to Starlog syntax
- **Starlog → Prolog**: Converts Starlog code back to standard Prolog

### 2. Variable Naming

- Pretty variable naming (A, B, C, etc.) instead of Prolog's default _123 style
- Consistent variable naming across conversions

### 3. Predicate Transformation

The converter transforms predicates between the two syntaxes:

#### String Operations
| Prolog | Starlog |
|--------|---------|
| `string_concat(A, B, C)` | `C is (A : B)` |
| `atom_concat(A, B, C)` | `C is (A • B)` |
| `string_length(A, B)` | `B is string_length(A)` |

#### List Operations
| Prolog | Starlog |
|--------|---------|
| `append(A, B, C)` | `C is (A & B)` |
| `length(A, B)` | `B is length_1(A)` |
| `reverse(A, B)` | `B is reverse(A)` |

#### Math Operations
| Prolog | Starlog |
|--------|---------|
| `Y is X * 2` | `Y is X * 2` (preserved) |

### 4. Comment Preservation

- Header comments are preserved during conversion
- Documentation comments are maintained in both formats

### 5. Syntax Handling

- Proper handling of quoted atoms and strings
- Preservation of fact and rule structure

## Usage

### Converting Prolog to Starlog

```bash
cd prolog_input
swipl -q -g main -s ../prolog_to_starlog_cli.pl
```

### Converting Starlog to Prolog

```bash
cd prolog_input
swipl -q -g main -s ../starlog_to_prolog_cli.pl 
```