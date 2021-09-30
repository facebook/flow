# `./tool test`

`./tool` is a script in the root of this project. `./tool test` runs the tests in this directory.

To make it work: run `yarn install` in the flow directory. This is needed both for `tool` to run, and also for `flow check` to work in the newtests directory.

## Motivation behind `./tool test`

* Tests should pair small examples with the expectations for each example.
* We should dogfood Flow

## Example test

Check out [tool_test_example](https://github.com/facebook/flow/blob/master/newtests/tool_test_example/test.js), which is an example test.

## Structure of a test

* A test is a file named `./**/test.js`.
* Each `test.js` file exports a `Suite` by default
* Each `Suite` contains a list of `Test`s.
* Each `Test` contains a list of `TestSteps`

### Exporting a `Suite`

The only way to create a `Suite` is to call the `suite()` function. The `suite()` function takes a callback, like so

```JavaScript
import {suite} from 'flow-dev-tools/src/test/Tester';
import type TestStep from 'flow-dev-tools/src/test/TestStep';
module.exports = suite((emptyTestStep: TestStep) => [ < List of Tests >]);
```

(Why the `suite()` function? Why not just export the callback directly? Well, it removes the need for type annotations!)

### Creating a `Test`

The only way to create a `Test` is to call the `test()` function. The `test()` function takes a test name and a list of `TestStep`s, like so

```JavaScript
const {suite, test} = require('flow-dev-tools/src/test/Tester');
import type TestStep from 'flow-dev-tools/src/test/TestStep';
module.exports = suite((emptyTestStep: TestStep) => [
  test('My first test, [ < List of TestSteps > ]'),
]);
```

### `TestStep`s

A `TestStep` is made up of 0 or more actions and 0 or more assertions. The `emptyTestStep` passed to `suite()`'s callback is a `TestStep` with 0 actions and 0 assertions. `TestStep`s are immutable, so when you call `emptyTestStep.addFile('foo.js')` you get back a new `TestStep` with 1 action and 0 assertions. So a test looks like

```JavaScript
const {suite, test} = require('flow-dev-tools/src/test/Tester');
import type TestStep from 'flow-dev-tools/src/test/TestStep';
module.exports = suite((emptyTestStep: TestStep) => [
  test('My first test', [
    emptyTestStep
      .addCode('var x = 123')
      .noNewErrors(),
    emptyTestStep
      .addCode('var y = "hello"')
      .noNewErrors(),
  ]),
]);
```

More concisely, this can be written

```JavaScript
const {suite, test} = require('flow-dev-tools/src/test/Tester');
module.exports = suite(({addCode}) => [
  test('My first test', [
    addCode('var x = 123')
      .noNewErrors(),
    addCode('var y = "hello"')
      .noNewErrors(),
  ]),
]);
```

Note: You cannot add actions to a `TestStep` after an assertion because @gabelevi felt like messing around with the type system to prevent it :)
