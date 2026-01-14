.PHONY: build test test-integration test-ocaml-compat test-all clean fmt

# Build the project
build:
	dune build

# Run unit tests (ppx_expect)
test:
	dune test

# Run integration tests
test-integration:
	./test/integration/run_tests.sh

# Run OCaml compatibility tests
test-ocaml-compat:
	./test/ocaml_compatibility/run_comparison.sh

# Run ALL tests (unit + integration + OCaml compatibility)
test-all: test test-integration test-ocaml-compat

# Remove build artifacts
clean:
	dune clean

# Format code
fmt:
	dune fmt
