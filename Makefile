.PHONY: build test test-integration test-all clean fmt

# Build the project
build:
	dune build

# Run unit tests (ppx_expect)
test:
	dune test

# Run integration tests
test-integration:
	./test/integration/run_tests.sh

# Run all tests
test-all: test test-integration

# Remove build artifacts
clean:
	dune clean

# Format code
fmt:
	dune fmt
