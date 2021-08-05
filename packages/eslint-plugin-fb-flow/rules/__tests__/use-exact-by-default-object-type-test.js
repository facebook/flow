/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @emails oncall+flow
 * @format
 */

'use strict';

const runTest = require('../../run-test');

runTest('use-exact-by-default-object-type', {
  valid: [
    'type Props = { foo: string };',

    'type Props = { foo: string, ... };',

    `// flowlint ambiguous-object-type:error
      type Props = {| foo: string |};`,
  ],
  invalid: [
    {
      code: 'type Props = {| foo: string |};',
      output: 'type Props = { foo: string };',
      errors: 1,
    },
  ],
});
