/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import {testCodemod} from '../../../testUtils/codemodTestUtils';
import replaceTemporaryTypes from '../replaceTemporaryTypes.js';

testCodemod('replaceTemporaryTypes', replaceTemporaryTypes, {
  ignored: [],
  transformed: [
    {
      description: 'temporary object',
      code: `type T = $TEMPORARY$object<{prop: string}>;`,
      output: `type T = $ReadOnly<{prop: string}>;`,
    },
    {
      description: 'temporary array',
      code: `type T = $TEMPORARY$array<number>;`,
      output: `type T = $ReadOnlyArray<number>;`,
    },
    {
      description: 'temporary string',
      code: `type T = $TEMPORARY$string<'foo'>;`,
      output: `type T = string;`,
    },
    {
      description: 'temporary number',
      code: `type T = $TEMPORARY$number<1>;`,
      output: `type T = number;`,
    },
    {
      description: 'nested temporary types',
      code: `type T = $TEMPORARY$array<$TEMPORARY$object<{prop: $TEMPORARY$string<"blah">}>>;`,
      output: `type T = $ReadOnlyArray<$ReadOnly<{prop: string}>>;`,
    },
  ],
});
