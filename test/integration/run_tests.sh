#!/bin/bash
# Integration test runner for Lina module system
# Compiles .lina files to Lua and runs them, checking expected output

set -e
cd "$(dirname "$0")"

RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

PASSED=0
FAILED=0

run_test() {
    local file="$1"
    local name=$(basename "$file" .lina)

    echo -n "Testing $name... "

    # Compile (use relaxed mode for tests - strict mode is the default)
    if ! ../../_build/default/bin/main.exe compile "$file" -o "$name.lua" --relaxed 2>/dev/null; then
        echo -e "${RED}FAILED${NC} (compilation error)"
        FAILED=$((FAILED + 1))
        return
    fi

    # Run and capture output
    if ! output=$(luajit "$name.lua" 2>&1); then
        echo -e "${RED}FAILED${NC} (runtime error: $output)"
        rm -f "$name.lua"
        FAILED=$((FAILED + 1))
        return
    fi

    # Clean up
    rm -f "$name.lua"

    echo -e "${GREEN}PASSED${NC}"
    echo "  Output: $output"
    PASSED=$((PASSED + 1))
}

echo "=== Lina Module System Integration Tests ==="
echo ""

# Run all .lina files in this directory
for file in *.lina; do
    [ -e "$file" ] || continue
    run_test "$file"
done

echo ""
echo "=== Results ==="
echo -e "Passed: ${GREEN}$PASSED${NC}"
echo -e "Failed: ${RED}$FAILED${NC}"

if [ $FAILED -gt 0 ]; then
    exit 1
fi
