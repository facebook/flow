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
import renamePartial from '../renamePartial';

testCodemod('renamePartial', renamePartial, {
  // IGNORED
  ignored: [
    {
      description: 'variable',
      code: `const $Partial = 1;`,
    },
    {
      description: 'alias name',
      code: `type $Partial = 1;`,
    },
    {
      description: 'no type arguments',
      code: `type T = $Partial;`,
    },
  ],

  // TRANSFORMED
  transformed: [
    {
      description: 'type alias',
      code: `type T = $Partial<A>;`,
      output: `type T = Partial<A>;`,
    },
  ],
});
