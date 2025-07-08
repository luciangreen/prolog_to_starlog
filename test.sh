#!/bin/bash
# Starlog executable generated from test.sl
# Author: luciangreenPlease
# Date: 2025-07-08

# Convert .sl to temporary .pl file
TEMP_FILE="${BASH_SOURCE%/*}/test_temp.pl"

# Convert and run
swipl -g "
    consult('starlog_to_prolog.pl'),
    read_file_to_clauses('test.sl', Clauses0),
    include(is_clause, Clauses0, Clauses),
    maplist(starlog_to_pl, Clauses, PrologClauses),
    copy_file_header('test.sl', '$TEMP_FILE'),
    write_clauses_to_file('$TEMP_FILE', PrologClauses),
    consult('$TEMP_FILE'),
    delete_file('$TEMP_FILE')
" -t halt
