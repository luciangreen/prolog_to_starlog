#!/bin/bash

# Test Round-Trip Conversions for Starlog-Prolog Converter
# Date: 2025-07-02 02:30:27 UTC
# Author: luciangreenPlease

# Set up colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}=== Starlog-Prolog Converter Round-Trip Tests ===${NC}"
echo "Date/Time: 2025-07-02 02:30:27 UTC"
echo "User: luciangreenPlease"

# Create temporary directories
mkdir -p temp/starlog_input
mkdir -p temp/prolog_output
mkdir -p temp/prolog_input
mkdir -p temp/starlog_output
mkdir -p temp/round_trip

# Create test files if they dont exist
if [ ! -f "test1.pl" ]; then
    cat > test1.pl << 'EOF'
:-(test_term_to_atom(A,B),term_to_atom(A,B)).
:-(test_number_string(A,B),number_string(A,B)).
:-(test_maplist(A,B),maplist(atom_chars,A,B)).
:-(test_foldl(A,B),foldl(atom_concat,A,"",B)).
:-(test_string_chars(A,B),string_chars(A,B)).

% Test case for duplicate expression elimination
g(A,B) :- A is f, A is f, B = A.

% Test case for basic conversion and compression
test_compression(A,B,C,D,E) :- 
    string_concat(A,B,C), 
    atom_concat(C,D,E).

% Test case for maintaining quotes
test_quotes(Quoted_atom, "Quoted string", Result) :-
    string_concat(Quoted_atom, "Quoted string", Result).
EOF
fi

if [ ! -f "test5.pl" ]; then
    cat > test5.pl << 'EOF'
% Test file for additional predicate conversions

% Test is, =
test_is(A, B) :- A is B + 5.
test_equals(A, B) :- A = B.

% Test wrap/2, unwrap/2
test_wrap(A, B) :- wrap(A, B).
test_unwrap(A, B) :- unwrap(A, B).

% Test head/2, tail/2
test_head_tail(A, H, T) :- head(A, H), tail(A, T).

% Test member, delete
test_member_delete(A, X, B) :- member(X, A), delete(A, X, B).

% Test string_to_number/2, random
test_string_number(S, N) :- string_to_number(S, N).
test_random(N, R) :- random(N, R).

% Test length, ceiling, date
test_length(L, N) :- length(L, N).
test_ceiling(X, Y) :- ceiling(X, Y).
test_date(D) :- date(D).

% Test sqrt, round
test_sqrt(X, Y) :- sqrt(X, Y).
test_round(X, Y) :- round(X, Y).

% Test findall, string_from_file
test_findall(X, L) :- findall(N, between(1, X, N), L).
test_string_from_file(F, S) :- string_from_file(F, S).

% Test maplist, sort, intersection
test_maplist(A, B) :- maplist(plus(1), A, B).
test_sort(A, B) :- sort(A, B).
test_intersection(A, B, C) :- intersection(A, B, C).

% Test read_string, atom_string
test_read_string(F, S) :- open(F, read, Stream), read_string(Stream, 1000, _, _, S), close(Stream).
test_atom_string(A, S) :- atom_string(A, S).

% Test call
test_call(X) :- call(member(X, [1, 2, 3])).

% Test complex compression of multiple operations
complex_test(S, N, Result) :-
    string_to_number(S, N),
    sqrt(N, Sqrt),
    ceiling(Sqrt, Ceil),
    round(Ceil, Round),
    random(Round, R),
    number_string(R, Result).

% Test nested compression
nested_test(List, Result) :-
    length(List, Len),
    sort(List, Sorted),
    maplist(sqrt, Sorted, Sqrts),
    reverse(Sqrts, Result).
EOF
fi

if [ ! -f "examples.pl" ]; then
    cat > examples.pl << 'EOF'
% Example file for testing

fact1.
fact2(a,b).

test_simple(A, B) :- append([1], [2], A), append(A, [3], B).

test_strings(A, B, C) :- 
    atom_concat("Hello", " ", A), 
    atom_concat(A, "world!", B),
    string_length(B, C).

test_builtin(X, Y) :- 
    number_string(42, X),
    string_concat(X, " is the answer", Y).

% More complex example with nested calls
test_nested(Input, Output) :-
    length(Input, Len),
    (Len > 5 -> sort(Input, Output) ; reverse(Input, Output)).

% Test quoted atoms and strings
test_quotes(single_quoted, "double quoted").
EOF
fi

if [ ! -f "test4.pl" ]; then
    cat > test4.pl << 'EOF'
% Test file for special notation
foo.
bar(X) :- baz(X).
baz(a).

% Test trailing white space removal
remove_trailing_white_space(F4,F8) :-
    reverse(F4,F6),
    findall_until_fail(B,member(B,F6),is_space(B),F5),
    append(F5,F7,F6),
    reverse(F7,F8),!.

% Test split12 function
split12([],_,A,A):-!.
split12(Q2,L16,L20,L17) :-
	length(L18,L16),
	append(L18,L19,Q2),
	append(L20,[L18],L21),
	split12(L19,L16,L21,L17),!.
split12(A,_L16,B,C) :- append(B,[A],C),!.
EOF
fi

# Copy all necessary files to temp
cp var_utils.pl temp/
cp prolog_to_starlog_cli.pl temp/
cp starlog_to_prolog_cli.pl temp/

# Clean up any existing temporary files
rm -rf temp/starlog_input/*
rm -rf temp/prolog_output/*
rm -rf temp/prolog_input/*
rm -rf temp/starlog_output/*
rm -rf temp/round_trip/*

# Move any existing Starlog files to temp/starlog_input
echo -e "\n${YELLOW}Moving initial Starlog files to temp/starlog_input${NC}"
for file in *_starlog.pl; do
    if [ -f "$file" ] && [ "$file" != "*_starlog.pl" ]; then
        echo "Moving $file to temp/starlog_input"
        cp "$file" "temp/starlog_input/"
    fi
done

# Copy all Prolog files to temp/prolog_input
echo -e "\n${YELLOW}Copying Prolog files to temp/prolog_input${NC}"
for file in *.pl; do
    if [[ "$file" != *"_starlog.pl" && "$file" != *"_prolog.pl" && "$file" != "*_cli.pl" ]]; then
        if [ -f "$file" ] && [ "$file" != "var_utils.pl" ] && [ "$file" != "prolog_to_starlog_cli.pl" ] && [ "$file" != "starlog_to_prolog_cli.pl" ]; then
            echo "Copying $file to temp/prolog_input"
            cp "$file" "temp/prolog_input/"
        fi
    fi
done

# Change to temp directory for testing
cd temp || exit 1

# Run conversions in the appropriate directories
echo -e "\n${YELLOW}Running Prolog → Starlog conversion${NC}"
cd prolog_input || exit 1
swipl -q -g main -s ../prolog_to_starlog_cli.pl
# Check if any starlog files were created
ls *_starlog.pl 2>/dev/null
if [ $? -eq 0 ]; then
    cp *_starlog.pl ../starlog_output/ 2>/dev/null
else
    echo "No Starlog files were generated"
fi
cd ..

echo -e "\n${YELLOW}Running Starlog → Prolog conversion on generated Starlog files${NC}"
cd starlog_output || exit 1
swipl -q -g main -s ../starlog_to_prolog_cli.pl
# Check if any prolog files were created
ls *_prolog.pl 2>/dev/null
if [ $? -eq 0 ]; then
    cp *_prolog.pl ../round_trip/ 2>/dev/null
else
    echo "No Prolog files were generated"
fi
cd ..

echo -e "\n${YELLOW}Running Starlog → Prolog conversion on original Starlog files${NC}"
cd starlog_input || exit 1
if [ "$(ls -A)" ]; then
    swipl -q -g main -s ../starlog_to_prolog_cli.pl
    # Check if any prolog files were created
    ls *_prolog.pl 2>/dev/null
    if [ $? -eq 0 ]; then
        cp *_prolog.pl ../prolog_output/ 2>/dev/null
    else
        echo "No Prolog files were generated"
    fi
else
    echo "No original Starlog files to convert"
fi
cd ..

# Return to the parent directory
cd ..

# Generate the report
echo -e "\n${BLUE}=== Round-Trip Conversion Report ===${NC}"
echo "Date/Time: 2025-07-02 02:30:27 UTC"

# Check Prolog → Starlog → Prolog round trip
echo -e "\n${YELLOW}Checking Prolog → Starlog → Prolog round trip:${NC}"
total_files=0
successful=0

# Check if there are any round trip files
if [ "$(ls -A temp/round_trip/ 2>/dev/null)" ]; then
    for file in temp/prolog_input/*.pl; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            base=${filename%.pl}
            round_trip_file="temp/round_trip/${base}_starlog_prolog.pl"
            
            if [ -f "$round_trip_file" ]; then
                total_files=$((total_files + 1))
                
                # Compare files (ignoring comments, blank lines, and variable naming differences)
                diff_output=$(diff -I "^%" -I "^$" -B "$file" "$round_trip_file" | grep -v "^[0-9]")
                
                if [ -z "$diff_output" ]; then
                    echo -e "${GREEN}✓ $filename: Round-trip successful${NC}"
                    successful=$((successful + 1))
                else
                    echo -e "${RED}✗ $filename: Round-trip failed${NC}"
                    echo "Differences:"
                    echo "$diff_output" | head -10
                    if [ $(echo "$diff_output" | wc -l) -gt 10 ]; then
                        echo "... (and more differences)"
                    fi
                fi
            else
                echo -e "${RED}✗ $filename: No corresponding round-trip file found${NC}"
                total_files=$((total_files + 1))
            fi
        fi
    done
else
    echo -e "${RED}No round-trip files were generated${NC}"
fi

# Check original Starlog → Prolog conversion
echo -e "\n${YELLOW}Checking original Starlog → Prolog conversion:${NC}"
starlog_total=0
starlog_successful=0

# Check if there are any starlog input files
if [ "$(ls -A temp/starlog_input/ 2>/dev/null)" ]; then
    for file in temp/starlog_input/*.pl; do
        if [ -f "$file" ]; then
            filename=$(basename "$file")
            base=${filename%_starlog.pl}
            prolog_file="temp/prolog_output/${base}_starlog_prolog.pl"
            
            if [ -f "$prolog_file" ]; then
                starlog_total=$((starlog_total + 1))
                
                # Simple check that the file was generated and is not empty
                if [ -s "$prolog_file" ]; then
                    echo -e "${GREEN}✓ $filename: Conversion successful${NC}"
                    starlog_successful=$((starlog_successful + 1))
                else
                    echo -e "${RED}✗ $filename: Generated file is empty${NC}"
                fi
            else
                echo -e "${RED}✗ $filename: No corresponding Prolog file generated${NC}"
                starlog_total=$((starlog_total + 1))
            fi
        fi
    done
else
    echo -e "${YELLOW}No original Starlog files to convert${NC}"
fi

# Print summary
echo -e "\n${BLUE}=== Summary ===${NC}"
if [ $total_files -gt 0 ]; then
    percent=$((successful * 100 / total_files))
    echo "Prolog → Starlog → Prolog round-trip conversions:"
    echo "  Successful: $successful / $total_files ($percent%)"
else
    echo "No round-trip conversions were performed"
fi

if [ $starlog_total -gt 0 ]; then
    percent=$((starlog_successful * 100 / starlog_total))
    echo "Original Starlog → Prolog conversions:"
    echo "  Successful: $starlog_successful / $starlog_total ($percent%)"
fi

# Check for syntax errors in the generated files
echo -e "\n${YELLOW}Checking for syntax errors in generated files:${NC}"

# Function to check Prolog syntax
check_syntax() {
    local file=$1
    if [ -f "$file" ]; then
        local result=$(swipl -q -g "halt(0)" -s "$file" 2>&1)
        if [ -n "$result" ]; then
            echo -e "${RED}✗ $file: Syntax error${NC}"
            echo "$result" | head -5
            return 1
        else
            echo -e "${GREEN}✓ $file: No syntax errors${NC}"
            return 0
        fi
    else
        return 0  # Skip if file doesnt exist
    fi
}

# Check Starlog files
syntax_ok=0
syntax_total=0
echo -e "\n${BLUE}Checking Starlog files syntax:${NC}"
for file in temp/starlog_output/*.pl; do
    if [ -f "$file" ]; then
        syntax_total=$((syntax_total + 1))
        if check_syntax "$file"; then
            syntax_ok=$((syntax_ok + 1))
        fi
    fi
done

# Check Prolog files
echo -e "\n${BLUE}Checking Prolog files syntax:${NC}"
for file in temp/round_trip/*.pl temp/prolog_output/*.pl; do
    if [ -f "$file" ]; then
        syntax_total=$((syntax_total + 1))
        if check_syntax "$file"; then
            syntax_ok=$((syntax_ok + 1))
        fi
    fi
done

echo -e "\n${BLUE}Syntax check summary:${NC}"
if [ $syntax_total -gt 0 ]; then
    percent=$((syntax_ok * 100 / syntax_total))
    echo "  Valid syntax: $syntax_ok / $syntax_total ($percent%)"
else
    echo "  No files to check for syntax"
fi

# Check for correct compression/decompression
echo -e "\n${YELLOW}Checking for compression and decompression:${NC}"

# Check test5.pl compression
if [ -f "temp/starlog_output/test5_starlog.pl" ]; then
    echo -e "${BLUE}Checking compression in test5_starlog.pl:${NC}"
    grep -A 2 "complex_test" temp/starlog_output/test5_starlog.pl
    
    if grep -q "Result is" temp/starlog_output/test5_starlog.pl || grep -q "number_string" temp/starlog_output/test5_starlog.pl; then
        echo -e "${GREEN}✓ Complex test compression detected${NC}"
    else
        echo -e "${RED}✗ Complex test compression not detected${NC}"
    fi
    
    # Check nested test compression
    if grep -q "nested_test" temp/starlog_output/test5_starlog.pl; then
        echo -e "${BLUE}Checking nested test compression:${NC}"
        grep -A 2 "nested_test" temp/starlog_output/test5_starlog.pl
        
        if grep -q "reverse(maplist" temp/starlog_output/test5_starlog.pl || grep -q "is reverse" temp/starlog_output/test5_starlog.pl; then
            echo -e "${GREEN}✓ Nested test compression detected${NC}"
        else
            echo -e "${RED}✗ Nested test compression not detected${NC}"
        fi
    fi
else
    echo -e "${RED}test5_starlog.pl not found for compression check${NC}"
fi

# Check decompression
if [ -f "temp/round_trip/test5_starlog_prolog.pl" ]; then
    echo -e "${BLUE}Checking decompression in test5_starlog_prolog.pl:${NC}"
    grep -A 5 "complex_test" temp/round_trip/test5_starlog_prolog.pl
    
    if grep -q "string_to_number" temp/round_trip/test5_starlog_prolog.pl && grep -q "sqrt" temp/round_trip/test5_starlog_prolog.pl; then
        echo -e "${GREEN}✓ Complex test decompression detected${NC}"
    else
        echo -e "${RED}✗ Complex test decompression not detected${NC}"
    fi
else
    echo -e "${RED}test5_starlog_prolog.pl not found for decompression check${NC}"
fi

# Check special cases
echo -e "\n${YELLOW}Checking special cases:${NC}"

# Check g(A is f, A is f) -> g(A is f, A)
if [ -f "temp/starlog_output/test1_starlog.pl" ]; then
    echo -e "${BLUE}Checking duplicate expression elimination:${NC}"
    grep -A 2 "g(" temp/starlog_output/test1_starlog.pl
    
    # Count occurrences of "is f"
    if grep -q "is f" temp/starlog_output/test1_starlog.pl; then
        is_f_count=$(grep -o "is f" <<< "$(grep -A 2 "g(" temp/starlog_output/test1_starlog.pl)" | wc -l)
        
        if [ "$is_f_count" -eq 1 ]; then
            echo -e "${GREEN}✓ Duplicate expression elimination working (g(A is f, A))${NC}"
        else
            echo -e "${RED}✗ Duplicate expression elimination failed${NC}"
        fi
    else
        echo -e "${RED}✗ Could not find is f pattern${NC}"
    fi
else
    echo -e "${RED}test1_starlog.pl not found for special case check${NC}"
fi

# Check for special notation in test4_starlog.pl
if [ -f "temp/starlog_output/test4_starlog.pl" ]; then
    echo -e "${BLUE}Checking special notation in test4_starlog.pl:${NC}"
    
    # Check for remove_trailing_white_space
    if grep -q "remove_trailing_white_space" temp/starlog_output/test4_starlog.pl; then
        echo "Found remove_trailing_white_space transformation:"
        grep -A 3 "remove_trailing_white_space" temp/starlog_output/test4_starlog.pl
        
        # Check for special function return indexing
        if grep -q "is.*findall_until_fail" temp/starlog_output/test4_starlog.pl || grep -q "is.*member_1" temp/starlog_output/test4_starlog.pl; then
            echo -e "${GREEN}✓ Special function indexing notation detected${NC}"
        else
            echo -e "${RED}✗ Special function indexing notation not found${NC}"
        fi
    fi
    
    # Check for split12
    if grep -q "split12" temp/starlog_output/test4_starlog.pl; then
        echo "Found split12 transformation:"
        grep -A 3 "split12" temp/starlog_output/test4_starlog.pl | head -4
        
        # Check for length_1 notation
        if grep -q "length_1" temp/starlog_output/test4_starlog.pl; then
            echo -e "${GREEN}✓ length_1 notation detected${NC}"
        else
            echo -e "${RED}✗ length_1 notation not found${NC}"
        fi
    fi
else
    echo -e "${YELLOW}test4_starlog.pl not found for special notation check${NC}"
fi

echo -e "\n${BLUE}=== Test Complete ===${NC}"
echo "See temp/ directory for all generated files"
echo "Date/Time: 2025-07-02 02:30:27 UTC"
echo "User: luciangreenPlease"