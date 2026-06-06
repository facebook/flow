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

describe('keyof', () => {
  test('basic', async () => {
    expect(
      await format(`
        type T1 = keyof Foo;
      `),
    ).toMatchInlineSnapshot(`
      "type T1 = keyof Foo;
      "
    `);
  });

  test('keyof typeof', async () => {
    expect(
      await format(`
        type T2 = keyof typeof obj;
      `),
    ).toMatchInlineSnapshot(`
      "type T2 = keyof typeof obj;
      "
    `);
  });

  test('keyof in array type', async () => {
    expect(
      await format(`
        type T3 = (keyof Foo)[];
        type T4 = (keyof typeof obj)[];
      `),
    ).toMatchInlineSnapshot(`
      "type T3 = (keyof Foo)[];
      type T4 = (keyof typeof obj)[];
      "
    `);
  });

  test('keyof in indexed access type', async () => {
    expect(
      await format(`
        type T5 = (keyof Foo)['bar'];
        type T6 = (keyof typeof obj)['bar'];
      `),
    ).toMatchInlineSnapshot(`
     "type T5 = (keyof Foo)['bar'];
     type T6 = (keyof typeof obj)['bar'];
     "
    `);
  });

  test('keyof in union and intersection types', async () => {
    expect(
      await format(`
        type T7 = keyof Foo | string;
        type T8 = keyof Foo & string;
      `),
    ).toMatchInlineSnapshot(`
      "type T7 = keyof Foo | string;
      type T8 = keyof Foo & string;
      "
    `);
  });

  test('typeof in array and indexed access types', async () => {
    expect(
      await format(`
        type T9 = (typeof obj)[];
        type T10 = (typeof obj)['bar'];
      `),
    ).toMatchInlineSnapshot(`
     "type T9 = (typeof obj)[];
     type T10 = (typeof obj)['bar'];
     "
    `);
  });
});
