# flowtest

A utility for running tests for flow types.

You can confirm that your flow types work as you expect them to
by writing tests that expect errors using suppressions. This tool
checks these tests to make sure there are no unused suppressions.

To run this utility, use `flowtest /path/to/root`.

## Usage

`flowtest` will check for any files in a `__flowtests__` directory and any
file with the `-flowtest.js` suffix. If there are any unused suppressions in
any of those files, `flowtest` will exit with a non-zero exit code. `flowtest`
also prints information about each individual test to stdout.

### Example test
```
//@flow

import {onlyTakesTest} from './code.js';

// $FlowExpectedError
onlyTakesTest('notTest');
```

### Example output

```
[✗] FAIL: __tests__/test-project/a/b/c/d/__flowtests__/fail.js
[✓] PASS: __tests__/test-project/a/b/c/d/__flowtests__/pass.js
[✗] FAIL: __tests__/test-project/a/b/c/d/fail-flowtest.js
[✓] PASS: __tests__/test-project/a/b/c/d/pass-flowtest.js
[✗] FAIL: __tests__/test-project/fail-flowtest.js
[✓] PASS: __tests__/test-project/pass-flowtest.js
```
