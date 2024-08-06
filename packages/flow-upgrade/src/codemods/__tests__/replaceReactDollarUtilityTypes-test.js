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
import codemod from '../replaceReactDollarUtilityTypes';

testCodemod('replaceReactDollarUtilityTypes', codemod, {
  transformed: [
    {
      description: 'changed',
      code: `type A = React$ElementProps<typeof Foo>;`,
      output: `type A = React.ElementProps<typeof Foo>;`,
    },
  ],
  ignored: [
    {
      description: 'irrelevant',
      code: 'const x: number = 1;',
    },
    {
      description: 'unsupported',
      code: `type A = React$AiHallucinatedUtilityType<string>;`,
    },
    {
      description: 'already good',
      code: `type A = React.Node;`,
    },
  ],
});
