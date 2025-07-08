#!/bin/bash
# Compile Starlog (.sl) files into standalone executables
# Author: luciangreenPlease
# Date: 2025-07-08 23:20:46 UTC

if [ $# -eq 0 ]; then
    echo "Usage: $0 <starlog_file.sl> [output_name]"
    echo "Example: $0 math_operations.sl math_calc"
    exit 1
fi

STARLOG_FILE="$1"
BASENAME=$(basename "$STARLOG_FILE" .sl)
OUTPUT_NAME="${2:-$BASENAME}"

# Check if file exists
if [ ! -f "$STARLOG_FILE" ]; then
    echo "Error: File '$STARLOG_FILE' not found"
    exit 1
fi

# Check if file has .sl extension
if [[ "$STARLOG_FILE" != *.sl ]]; then
    echo "Warning: File '$STARLOG_FILE' does not have .sl extension"
fi

echo "=== Compiling Starlog File: $STARLOG_FILE ==="
echo "Date/Time: $(date -u '+%Y-%m-%d %H:%M:%S UTC')"
echo "Output: $OUTPUT_NAME"
echo

# Define operators for Starlog syntax
OPERATORS="
:- op(700, xfx, is).
:- op(600, xfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').
"

# Create temporary compilation file
TEMP_FILE=$(mktemp /tmp/starlog_compile_XXXXXX.pl)
echo "$OPERATORS" > "$TEMP_FILE"
echo "" >> "$TEMP_FILE"
cat "$STARLOG_FILE" >> "$TEMP_FILE"
echo "" >> "$TEMP_FILE"
echo ":- initialization(main, main)." >> "$TEMP_FILE"
echo "" >> "$TEMP_FILE"
echo "main :- " >> "$TEMP_FILE"
echo "    format('~n=== Starlog Program: $STARLOG_FILE ===~n')," >> "$TEMP_FILE"
echo "    format('Available predicates: ')," >> "$TEMP_FILE"
echo "    current_predicate(F/A)," >> "$TEMP_FILE"
echo "    \\+ predicate_property(F/A, built_in)," >> "$TEMP_FILE"
echo "    format('~w/~w ', [F, A])," >> "$TEMP_FILE"
echo "    fail." >> "$TEMP_FILE"
echo "main :- " >> "$TEMP_FILE"
echo "    format('~n~nEnter goals interactively or use halt. to exit~n')," >> "$TEMP_FILE"
echo "    prolog." >> "$TEMP_FILE"

# Compile with SWI-Prolog
echo "Compiling with SWI-Prolog..."
swipl -o "$OUTPUT_NAME" -c "$TEMP_FILE"

if [ $? -eq 0 ]; then
    echo "Successfully created executable: $OUTPUT_NAME"
    echo "Run with: ./$OUTPUT_NAME"
else
    echo "Compilation failed"
    rm -f "$TEMP_FILE"
    exit 1
fi

# Clean up
rm -f "$TEMP_FILE"

echo
echo "=== Starlog Compilation Complete ==="