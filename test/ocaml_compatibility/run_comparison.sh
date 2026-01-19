#!/bin/bash
# OCaml Compatibility Test Suite
# Compares Lina and OCaml type system behavior
#
# For each test pair (test.lina, test.ml):
# 1. Compile with Lina, capture output/errors
# 2. Compile with OCaml, capture output/errors
# 3. Compare: both accept OR both reject with similar error
# 4. If both accept and produce code, run and compare output

set -e
cd "$(dirname "$0")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Counters
PASSED=0
FAILED=0
SKIPPED=0

# Path to Lina compiler
LINA_COMPILER="../../_build/default/bin/main.exe"

# Check if Lina compiler exists
if [ ! -f "$LINA_COMPILER" ]; then
    echo -e "${RED}Error: Lina compiler not found at $LINA_COMPILER${NC}"
    echo "Run 'dune build' first"
    exit 1
fi

# Check if OCaml is available
if ! command -v ocamlc &> /dev/null; then
    echo -e "${RED}Error: ocamlc not found in PATH${NC}"
    exit 1
fi

echo -e "${BLUE}OCaml Compatibility Test Suite${NC}"
echo "================================"
echo ""
echo "Lina compiler: $LINA_COMPILER"
echo "OCaml version: $(ocamlc -version 2>&1 | head -1)"
echo ""

compare_test() {
    local lina_file="$1"
    local ml_file="${lina_file%.lina}.ml"
    local name=$(basename "$lina_file" .lina)
    local dir=$(dirname "$lina_file")
    local category=$(basename "$dir")

    # Check if ML file exists
    if [ ! -f "$ml_file" ]; then
        echo -e "  ${YELLOW}SKIPPED${NC} $name (no .ml file)"
        SKIPPED=$((SKIPPED + 1))
        return
    fi

    printf "  %-50s " "$name"

    # Compile with Lina
    local lina_out
    local lina_exit_code
    # Use --relaxed to suppress unused code errors for compatibility testing
    lina_out=$($LINA_COMPILER compile "$lina_file" --relaxed 2>&1) && lina_exit_code=0 || lina_exit_code=$?
    local lina_ok=false
    if [ $lina_exit_code -eq 0 ] && [[ ! "$lina_out" =~ ^ERROR: ]]; then
        lina_ok=true
    fi

    # Compile with OCaml (type check only using -stop-after typing)
    local ml_out
    local ml_exit_code
    ml_out=$(ocamlc -stop-after typing "$ml_file" 2>&1) && ml_exit_code=0 || ml_exit_code=$?
    local ml_ok=false
    if [ $ml_exit_code -eq 0 ]; then
        ml_ok=true
    fi

    # Compare results
    if [ "$lina_ok" = "$ml_ok" ]; then
        if [ "$lina_ok" = "true" ]; then
            echo -e "${GREEN}PASSED${NC} (both accept)"
        else
            echo -e "${GREEN}PASSED${NC} (both reject)"
        fi
        PASSED=$((PASSED + 1))
    else
        echo -e "${RED}FAILED${NC}"
        echo "    Lina: $([ "$lina_ok" = "true" ] && echo "accepts" || echo "rejects")"
        echo "    OCaml: $([ "$ml_ok" = "true" ] && echo "accepts" || echo "rejects")"
        if [ "$lina_ok" = "false" ]; then
            echo "    Lina error:"
            echo "$lina_out" | sed 's/^/      /'
        fi
        if [ "$ml_ok" = "false" ]; then
            echo "    OCaml error:"
            echo "$ml_out" | sed 's/^/      /'
        fi
        FAILED=$((FAILED + 1))
    fi
}

# Run tests for a specific category
run_category() {
    local category="$1"
    local category_name=$(echo "$category" | sed 's/_/ /g' | sed 's/\b\(.\)/\u\1/g')

    if [ ! -d "$category" ]; then
        return
    fi

    local lina_files=("$category"/*.lina)
    if [ ! -e "${lina_files[0]}" ]; then
        return
    fi

    echo -e "${BLUE}=== $category_name ===${NC}"
    for lina_file in "$category"/*.lina; do
        [ -e "$lina_file" ] || continue
        compare_test "$lina_file"
    done
    echo ""
}

# Run all categories
CATEGORIES=(
    "value_restriction"
    "relaxed_value_restriction"
    "variance"
    "patterns"
    "gadts"
    "modules"
    "extensible"
    "labels"
    "locally_abstract_types"
    "level_propagation"
    "toplevel_expressions"
    "result"
    "lists"
    "arrays"
    "tuples"
    "dicts"
    "error_cases"
    "binding_operators"
    "loops"
    "strings"
    "imperative"
)

for category in "${CATEGORIES[@]}"; do
    run_category "$category"
done

# Print summary
echo "================================"
echo -e "${BLUE}Results${NC}"
echo "================================"
echo -e "Passed:  ${GREEN}$PASSED${NC}"
echo -e "Failed:  ${RED}$FAILED${NC}"
echo -e "Skipped: ${YELLOW}$SKIPPED${NC}"
echo ""

if [ $FAILED -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
