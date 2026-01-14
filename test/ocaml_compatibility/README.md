# OCaml Compatibility Test Suite

This test suite verifies that Lina's type system behaves identically to OCaml's. Tests are written in pairs (`.lina` and `.ml` files) and a test runner compares their type-checking behavior.

## Goal

Prove that Lina's type system is as sound and reliable as OCaml's by verifying:
- Both compilers accept the same valid programs
- Both compilers reject the same invalid programs

## Running Tests

```bash
# From the test/ocaml_compatibility directory
./run_comparison.sh

# From the project root
./test/ocaml_compatibility/run_comparison.sh
```

### Requirements

- Lina compiler built: `dune build`
- OCaml 5.x installed and available in PATH

## Test Categories

### Value Restriction (10 tests)

Tests the classic Wright 1995 value restriction - only syntactic values can be generalized.

| Test | Description | Expected |
|------|-------------|----------|
| `lambda_generalized` | Lambda is a value, generalized | ACCEPT |
| `application_not_generalized` | Application is not a value | ACCEPT (both behavior same) |
| `partial_application` | Partial app is a value | ACCEPT |
| `ref_not_polymorphic` | ref is invariant | REJECT |
| `tuple_of_values` | Tuple of values is a value | ACCEPT |
| `constructor_value` | Constructor of value is a value | ACCEPT |
| `let_in_value` | Let-in with value body is a value | ACCEPT |
| `match_not_value` | Match is not a value | REJECT |
| `if_not_value` | If is not a value | REJECT |
| `sequence_not_value` | Sequence is not a value | REJECT |

### Relaxed Value Restriction (6 tests)

Tests Garrigue 2004 relaxed value restriction - covariant type variables can be generalized even for non-values.

| Test | Description | Expected |
|------|-------------|----------|
| `covariant_generalization` | Covariant type vars generalized | ACCEPT |
| `contravariant_blocked` | Contravariant blocks generalization | REJECT |
| `invariant_blocked` | Invariant (ref) blocks generalization | REJECT |
| `nested_covariance` | Nested covariant types | ACCEPT |
| `function_result_covariant` | Function result is covariant | ACCEPT |
| `bivariant_generalized` | Unused (bivariant) type vars | ACCEPT |

### Variance (7 tests)

Tests variance inference for type constructors.

| Test | Description | Expected |
|------|-------------|----------|
| `list_covariant` | List is covariant | ACCEPT |
| `ref_invariant` | Ref is invariant | REJECT |
| `function_arg_contravariant` | Function arg is contravariant | REJECT |
| `function_result_covariant` | Function result is covariant | ACCEPT |
| `tuple_covariant` | Tuple is covariant | ACCEPT |
| `record_mutable_invariant` | Mutable fields are invariant | REJECT |
| `explicit_variance_check` | Explicit variance annotation | ACCEPT |

### GADTs (8 tests)

Tests Generalized Algebraic Data Types.

| Test | Description | Expected |
|------|-------------|----------|
| `basic_gadt_syntax` | GADT definition syntax | ACCEPT |
| `type_refinement` | Type refinement in branches | ACCEPT |
| `exhaustiveness_type_aware` | Type-aware exhaustiveness | ACCEPT |
| `polymorphic_recursion` | `type a.` annotation | ACCEPT |
| `existential_escape` | Existential type escape | REJECT |
| `gadt_eval_function` | Classic GADT evaluator | ACCEPT |
| `nested_gadt_match` | Nested GADT patterns | ACCEPT |
| `gadt_with_constraints` | Type equality witness | ACCEPT |

### Modules (9 tests)

Tests the module system.

| Test | Description | Expected |
|------|-------------|----------|
| `basic_module` | Simple module definition | ACCEPT |
| `module_type_annotation` | Module with signature | ACCEPT |
| `strengthening` | Module alias preserves equality | ACCEPT |
| `with_type_constraint` | `S with type t = int` | ACCEPT |
| `functor_basic` | Simple functor | ACCEPT |
| `functor_application` | Functor application | ACCEPT |
| `applicative_functor` | F(M).t = F(M).t | ACCEPT |
| `signature_mismatch` | Module doesn't match sig | REJECT |
| `abstract_type_escape` | Abstract type used as concrete | REJECT |

### Locally Abstract Types (5 tests)

Tests locally abstract type syntax and semantics.

| Test | Description | Expected |
|------|-------------|----------|
| `basic_syntax` | `(type a)` syntax | ACCEPT |
| `multiple_types` | Multiple type parameters | ACCEPT |
| `in_let_binding` | Local type in let | ACCEPT |
| `with_annotation` | `type a.` annotation | ACCEPT |
| `rigid_unification` | Rigid type doesn't unify | REJECT |

### Level Propagation (4 tests)

Tests type variable level tracking for let-polymorphism.

| Test | Description | Expected |
|------|-------------|----------|
| `nested_let_levels` | Nested lets assign levels | ACCEPT |
| `unification_level_update` | Unification updates levels | ACCEPT |
| `deep_nesting` | Deeply nested lets | ACCEPT |
| `mutual_recursion_levels` | Mutual recursion levels | ACCEPT |

### Error Cases (8 tests)

Tests that both compilers reject the same invalid programs.

| Test | Description | Expected |
|------|-------------|----------|
| `occurs_check` | Infinite type | REJECT |
| `type_mismatch` | int + bool | REJECT |
| `unbound_type` | Undefined type | REJECT |
| `unbound_value` | Undefined value | REJECT |
| `arity_mismatch` | Too many arguments | REJECT |
| `constructor_mismatch` | Wrong constructor arity | REJECT |
| `record_field_missing` | Missing required field | REJECT |
| `duplicate_field` | Duplicate record field | REJECT |

## Syntax Differences

Lina and OCaml have minor syntax differences that tests account for:

| Feature | Lina | OCaml |
|---------|------|-------|
| Print int | `print x` | `print_int x` |
| Tuple in GADT | `Add : (a * b) -> t` | `Add : a * b -> t` |
| List type | Must define | Built-in |
| Option type | Must define | Built-in |
| List literals | Not available | `[1; 2; 3]` |

**Important**: Lina has no standard library. Tests must define their own `list`, `option`, and similar types.

## Test File Format

Each test is a pair of files with the same base name:
- `test_name.lina` - Lina source code
- `test_name.ml` - Equivalent OCaml source code

Files include comments explaining:
- What the test verifies
- Expected result (ACCEPT or REJECT)

## Test Runner Output

The test runner produces colored output:
- **GREEN PASSED**: Both compilers agree (both accept or both reject)
- **RED FAILED**: Compilers disagree
- **YELLOW SKIPPED**: Missing `.ml` file

Example output:
```
=== Value Restriction ===
  lambda_generalized                                PASSED (both accept)
  ref_not_polymorphic                               PASSED (both reject)

=== Results ===
Passed:  57
Failed:  0
Skipped: 0

All tests passed!
```

## Adding New Tests

1. Create paired files in the appropriate category directory
2. Include comments explaining the test purpose and expected result
3. Ensure both files are semantically equivalent (accounting for syntax differences)
4. Run the test suite to verify the test works correctly

## Test Count

| Category | Tests |
|----------|-------|
| Value Restriction | 10 |
| Relaxed Value Restriction | 6 |
| Variance | 7 |
| GADTs | 8 |
| Modules | 9 |
| Locally Abstract Types | 5 |
| Level Propagation | 4 |
| Error Cases | 8 |
| **Total** | **57** |
