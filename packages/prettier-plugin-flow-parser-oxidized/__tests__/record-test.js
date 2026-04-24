/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

// $FlowExpectedError[cannot-resolve-module]
import prettierConfig from '../../.prettierrc.json';

import * as prettier from 'prettier';

function format(code: string) {
  const options = {
    ...prettierConfig,
    parser: 'hermes',
    requirePragma: false,
    plugins: [require.resolve('../index.mjs')],
  };
  return prettier.format(code, options);
}

describe('Records', () => {
  test('basic', async () => {
    expect(
      await format(`
       record R {
         foo: number,
       }
       const x = R {foo: 1};
      `),
    ).toMatchInlineSnapshot(`
     "record R {
       foo: number,
     }
     const x = R {foo: 1};
     "
    `);
  });
});
