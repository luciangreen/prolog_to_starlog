#!/bin/bash
# Run Starlog (.sl) files in SWI-Prolog
# Author: luciangreenPlease
# Date: 2025-07-08 23:20:46 UTC

if [ $# -eq 0 ]; then
    echo "Usage: $0 <starlog_file.sl> [goal]"
    echo "Example: $0 math_operations.sl 'double(5, X)'"
    exit 1
fi

STARLOG_FILE="$1"
GOAL="${2:-true}"

# Check if file exists
if [ ! -f "$STARLOG_FILE" ]; then
    echo "Error: File '$STARLOG_FILE' not found"
    exit 1
fi

# Check if file has .sl extension
if [[ "$STARLOG_FILE" != *.sl ]]; then
    echo "Warning: File '$STARLOG_FILE' does not have .sl extension"
fi

echo "=== Running Starlog File: $STARLOG_FILE ==="
echo "Date/Time: $(date -u '+%Y-%m-%d %H:%M:%S UTC')"
echo "Goal: $GOAL"
echo

# Define operators for Starlog syntax
OPERATORS="
:- op(700, xfx, is).
:- op(600, xfx, ':').
:- op(500, xfx, '&').
:- op(500, xfx, '•').
"

# Create temporary file with operators
TEMP_FILE=$(mktemp /tmp/starlog_run_XXXXXX.pl)
echo "$OPERATORS" > "$TEMP_FILE"
echo "" >> "$TEMP_FILE"
cat "$STARLOG_FILE" >> "$TEMP_FILE"

# Run with SWI-Prolog
echo "Running SWI-Prolog..."
if [ "$GOAL" = "true" ]; then
    echo "Loading file in interactive mode..."
    swipl -q -f "$TEMP_FILE"
else
    echo "Executing goal: $GOAL"
    swipl -q -f "$TEMP_FILE" -g "$GOAL" -t halt
fi

# Clean up
rm -f "$TEMP_FILE"

echo
echo "=== Starlog Execution Complete ==="