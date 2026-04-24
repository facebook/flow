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

describe(`'const' type parameters`, () => {
  test('type', async () => {
    expect(
      await format(`
        type T<const X> = X;
      `),
    ).toMatchInlineSnapshot(`
      "type T<const X> = X;
      "
    `);
  });

  test('function: basic', async () => {
    expect(
      await format(`
        function f<const T>(): void {}
      `),
    ).toMatchInlineSnapshot(`
      "function f<const T>(): void {}
      "
    `);
  });

  test('function: with variance', async () => {
    expect(
      await format(`
        function f<const +T>(): void {}
      `),
    ).toMatchInlineSnapshot(`
      "function f<const +T>(): void {}
      "
    `);
  });

  test('arrow: basic', async () => {
    expect(
      await format(`
        <const T>(x: T) => {}
      `),
    ).toMatchInlineSnapshot(`
      "<const T>(x: T) => {};
      "
    `);
  });

  test('arrow: with variance', async () => {
    expect(
      await format(`
        <const +T>(x: T) => {}
      `),
    ).toMatchInlineSnapshot(`
      "<const +T>(x: T) => {};
      "
    `);
  });

  test('class: basic', async () => {
    expect(
      await format(`
        class C<const T>{}
      `),
    ).toMatchInlineSnapshot(`
      "class C<const T> {}
      "
    `);
  });

  test('class: with variance', async () => {
    expect(
      await format(`
        class C<const T>{}
      `),
    ).toMatchInlineSnapshot(`
      "class C<const T> {}
      "
    `);
  });
});
