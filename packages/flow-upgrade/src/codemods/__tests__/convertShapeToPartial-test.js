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
import convertShapeToPartial from '../convertShapeToPartial';

testCodemod('convertShapeToPartial', convertShapeToPartial, {
  // IGNORED
  ignored: [
    {
      description: 'variable',
      code: `const $Shape = 1;`,
    },
    {
      description: 'alias name',
      code: `type $Shape = 1;`,
    },
    {
      description: 'no type arguments',
      code: `type T = $Shape;`,
    },
  ],

  // TRANSFORMED
  transformed: [
    {
      description: 'type alias',
      code: `type T = $Shape<A>;`,
      output: `type T = Partial<A>;`,
    },
  ],
});
