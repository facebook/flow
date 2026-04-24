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

function getOptions() {
  return {
    ...prettierConfig,
    parser: 'hermes',
    requirePragma: false,
    plugins: [require.resolve('../index.mjs')],
  };
}

describe('Component Declaration', () => {
  it('Formatting', async () => {
    const code = `
type T = component();

type T = component() renders SomeComponent;

type T = component() renders React.Element<typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng>;

type T = component<T>();

type T = component<T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>();

type T = component(bar: string);

type T = component(bar?: string);

type T = component('data-bar': string);

type T = component(...restProps: $ReadOnly<{k: string}>);

type T = component(...$ReadOnly<{k: string}>);

type T = component(bar: string, baz: $ReadOnly<{k: string}>);

type T = component(bar: string, baz: $ReadOnly<{k: string}>, realllllllllllllllllllyLong: string);

// Attached comment
type T = component(
  /**
   * Commet block
   */
  bar: string, // Trailing comment

  // preceding comment
  'data-baz': $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
);

type T = component(
  ...props: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
);

type T = component(
  ...props: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
) renders SomeComponent;

type T = component(bar: string) | component(baz: $ReadOnly<{k: string}>) | component(realllllllllllllllllllyLong: string, reallllllllllllllllllllllllllllllllllllllyLong: string);


function A(realllllllllllllllllllyLong: string, reallllllllllllllllllllllllllllllllllllllyLong: string): component(realllllllllllllllllllyLong: string, reallllllllllllllllllllllllllllllllllllllyLong: string) {}
    `;
    const output = await prettier.format(code, getOptions());
    expect(output).toMatchInlineSnapshot(`
      "type T = component();

      type T = component() renders SomeComponent;

      type T = component() renders React.Element<
        typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng,
      >;

      type T = component<T>();

      type T = component<
        T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo,
      >();

      type T = component(bar: string);

      type T = component(bar?: string);

      type T = component('data-bar': string);

      type T = component(...restProps: $ReadOnly<{k: string}>);

      type T = component(...$ReadOnly<{k: string}>);

      type T = component(bar: string, baz: $ReadOnly<{k: string}>);

      type T = component(
        bar: string,
        baz: $ReadOnly<{k: string}>,
        realllllllllllllllllllyLong: string,
      );

      // Attached comment
      type T = component(
        /**
         * Commet block
         */
        bar: string, // Trailing comment

        // preceding comment
        'data-baz': $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>,
        // Trailing comment
      );

      type T = component(
        ...props: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>
        // Trailing comment
      );

      type T = component(
        ...props: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>
        // Trailing comment
      ) renders SomeComponent;

      type T =
        | component(bar: string)
        | component(baz: $ReadOnly<{k: string}>)
        | component(
            realllllllllllllllllllyLong: string,
            reallllllllllllllllllllllllllllllllllllllyLong: string,
          );

      function A(
        realllllllllllllllllllyLong: string,
        reallllllllllllllllllllllllllllllllllllllyLong: string,
      ): component(
        realllllllllllllllllllyLong: string,
        reallllllllllllllllllllllllllllllllllllllyLong: string,
      ) {}
      "
    `);
  });
});
