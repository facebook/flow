# Flow Test Suite

This directory contains the test suite for Flow. Each subdirectory represents a test case with source files and expected outputs.

For all commands described below, they must be run from the project root (flow).

## Quick Start

### OSS Build (using Make)

```bash
# Build Flow
make

# Run all tests
./runtests.sh bin/flow

# Run a specific test
./runtests.sh -t <test_name> bin/flow

# Example: run the records test
./runtests.sh -t records bin/flow
```

### Meta Internal Development (using Buck)

```bash
./runtests.sh -t mytest $(buck2 build //flow:flow --show-full-output | awk '{print $2}')
```

## Common Commands

### Run tests matching a pattern
```bash
./runtests.sh <flow_binary> -f "^record"  # All tests starting with "record"
```

### List tests without running them
```bash
./runtests.sh -l <flow_binary>
```

### Update expected output after intentional changes
```bash
./runtests.sh -t <test_name> -r <flow_binary>
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

## Common Issues

### "No such file or directory" for flow binary

Make sure you're using the full absolute path to the flow binary:

```bash
# Wrong (relative path may fail)
./runtests.sh -t mytest ../bin/flow

# Correct (absolute path)
./runtests.sh -t mytest /full/path/to/bin/flow

# Or use command substitution (Meta only)
./runtests.sh -t mytest $(buck2 build //flow:flow --show-full-output | awk '{print $2}')
```
