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

describe('Hook Declaration', () => {
  it('Formatting', async () => {
    const code = `
declare hook useFoo(): void;

declare hook useFoo(): SomeComponent;

declare hook useFoo(): React.Element<typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng>;

declare hook useFoo<T>(): void;

declare hook useFoo<T>(): Array<Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>;

declare hook useFoo<T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>(): void;

declare hook useFoo(bar: string): string;

declare hook useFoo(bar?: string): string;

declare hook useFoo(data: string): string;

declare hook useFoo(...restProps: $ReadOnly<{k: string}>): {k: string};

declare hook useFoo(...$ReadOnly<{k: string}>): {k: string};

declare hook useFoo(bar: string, baz: $ReadOnly<{k: string}>): void;

declare hook useFoo(bar: string, baz: $ReadOnly<{k: string}>, realllllllllllllllllllyLong: string): void;

// Attached comment
declare hook useFoo(
  /**
   * Commet block
   */
  bar: string, // Trailing comment

  // preceding comment
  baz: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
): void;

declare hook useFoo(
  ...props: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
): SomeComponent;

declare hook useFoo(
  ...props: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
): SomeComponent;
    `;
    const output = await prettier.format(code, getOptions());
    expect(output).toMatchInlineSnapshot(`
      "declare hook useFoo(): void;

      declare hook useFoo(): SomeComponent;

      declare hook useFoo(): React.Element<
        typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng,
      >;

      declare hook useFoo<T>(): void;

      declare hook useFoo<
        T,
      >(): Array<Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>;

      declare hook useFoo<
        T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo,
      >(): void;

      declare hook useFoo(bar: string): string;

      declare hook useFoo(bar?: string): string;

      declare hook useFoo(data: string): string;

      declare hook useFoo(...restProps: $ReadOnly<{k: string}>): {k: string};

      declare hook useFoo(...$ReadOnly<{k: string}>): {k: string};

      declare hook useFoo(bar: string, baz: $ReadOnly<{k: string}>): void;

      declare hook useFoo(
        bar: string,
        baz: $ReadOnly<{k: string}>,
        realllllllllllllllllllyLong: string,
      ): void;

      // Attached comment
      declare hook useFoo(
        /**
         * Commet block
         */
        bar: string, // Trailing comment

        // preceding comment
        baz: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>,
      ): // Trailing comment
      void;

      declare hook useFoo(
        ...props: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>
      ): // Trailing comment
      SomeComponent;

      declare hook useFoo(
        ...props: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>
      ): // Trailing comment
      SomeComponent;
      "
    `);
  });
});
