/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 * @oncall flow
 */

import {testCodemod} from '../../../testUtils/codemodTestUtils';
import codemod from '../typeCastToAsExpression';

testCodemod('typeCastToAsExpression', codemod, {
  transformed: [
    {
      description: 'basic',
      code: `(1: number);`,
      output: `1 as number;`,
    },
    {
      description: 'parens added if needed',
      code: `(xs: Array<number>)[0];`,
      output: `(xs as Array<number>)[0];`,
    },
    {
      description: 'nested',
      code: `((1: any): string);`,
      output: `1 as any as string;`,
    },
    {
      description: 'nested deep - only first level updated',
      code: `(foo(() => (1: number)): string);`,
      output: `foo(() => (1: number)) as string;`,
    },
    {
      description: 'comments',
      code: `
// $FlowFixMe[code]
(1: number);`,
      output: `
// $FlowFixMe[code]
1 as number;`,
    },
  ],
  ignored: [
    {
      description: 'random other code is not affected',
      code: 'const x: number = 1;',
    },
  ],
});
