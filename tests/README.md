# Flow Test Suite

This directory contains the test suite for Flow. Each subdirectory represents a test case with source files and expected outputs.

For all commands described below, they must be run from the project root (flow).

These tests are run by the Rust test runner, `<flow> dev-tools runtests`, where
`<flow>` is the path to a Flow binary.

## Quick Start

### OSS Build (using Make)

```bash
# Build Flow
make

# Run all tests
bin/flow dev-tools runtests

# Run a specific test
bin/flow dev-tools runtests -t <test_name>

# Example: run the records test
bin/flow dev-tools runtests -t records
```

### Meta Internal Development (using Buck)

```bash
$(buck2 build //flow:flow --show-full-output | awk '{print $2}') dev-tools runtests -t mytest
```

## Common Commands

### Run tests matching a pattern
```bash
<flow_binary> dev-tools runtests -f "^record"  # All tests starting with "record"
```

### List tests without running them
```bash
<flow_binary> dev-tools runtests -l
```

### Update expected output after intentional changes
```bash
<flow_binary> dev-tools runtests -t <test_name> -r
```

This re-records the test output. Use this when you've intentionally changed Flow's behavior and need to update the `.exp` files.

## Test Structure

Each test directory contains:

- **Source files** (`.js`, `.flow`, etc.) - The JavaScript/Flow code being tested
- **`.exp` file** - Expected type errors and their format (human-readable)
- **`.out` file** - Generated output from the last test run (auto-generated, not checked in)
- **`.err` file** - Error output if the test failed (auto-generated)
- **`test.sh`** (optional) - Custom test script for tests with special requirements

## Tips

- **Always build Flow before running tests** - Stale binaries lead to confusing results
- **Use `-t` for single tests during development** - Faster iteration

## Adding New Tests

Each test is a directory under `tests/` containing source files and a `.flowconfig`.

### .flowconfig

Add `all=true` to the `.flowconfig` so that `// @flow` pragmas are not required in test `.js` files:

```
[options]
all=true
```

### Declaring variables with specific types

Use `declare const` to create a variable of a given type without needing a runtime value:

```js
declare const x: number;
declare const y: Map<string, number>;
```

Do not use `declare var`.

To reuse variable names, wrap declarations in blocks to scope them:

```js
{
  declare const x: number;
  // use x as number here
}
{
  declare const x: string;
  // use x as string here
}
```

### Casting syntax

Use `as` for type casts:

```js
x as T;
```

### Expecting errors

Add an end-of-line `// ERROR` comment on lines where you expect Flow to report an error:

```js
declare const x: number;
x as string; // ERROR
```

### Capturing a variable's type in the snapshot

To record the inferred type of a variable in the test snapshot, cast it to `empty` to force an error. The error output will include the variable's type:

```js
x as empty; // ERROR
```

## Common Issues

### "No such file or directory" for flow binary

Make sure you're using the full absolute path to the flow binary:

```bash
# Wrong (relative path may fail)
../bin/flow dev-tools runtests -t mytest

# Correct (absolute path)
/full/path/to/bin/flow dev-tools runtests -t mytest

# Or use command substitution (Meta only)
$(buck2 build //flow:flow --show-full-output | awk '{print $2}') dev-tools runtests -t mytest
```
