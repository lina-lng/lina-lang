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
    # Use full path (with slashes replaced) to avoid conflicts between same-named files
    local name=$(echo "$file" | sed 's|^\./||; s|\.lina$||; s|/|_|g')

    echo -n "Testing $file... "

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

# Find all .lina files recursively, excluding multifile/
for file in $(fd -e lina -E multifile . 2>/dev/null || find . -name "*.lina" -not -path "./multifile/*" | sort); do
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
