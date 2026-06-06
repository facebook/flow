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
type T = hook () => void;

type T = hook (bar) => SomeComponent;

type T = hook () => React.Element<typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng>;

type T = hook <T>() => void;

type T = hook <T>() => Array<Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>;

type T = hook <T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>() => void;

type T = hook (bar: string) => string;

type T = hook (bar?: string) => string;

type T = hook (data: string) => string;

type T = hook (...restProps: $ReadOnly<{k: string}>) => {k: string};

type T = hook (...$ReadOnly<{k: string}>) => {k: string};

type T = hook (bar: string, baz: $ReadOnly<{k: string}>) => void;

type T = hook (bar: string, baz: $ReadOnly<{k: string}>, realllllllllllllllllllyLong: string) => void;

type Banana = {
  useFoo: hook (string) => boolean,
};

interface F2 { m: hook (string) => number }

type f3 = hook (...arg) => void;

type f4 = hook (/* comment */ arg) => void;

type f5 = hook (arg /* comment */) => void;

type f6 = hook (?arg) => void;

class Y {
  constructor(
    ideConnectionFactory: hook (child_process$ChildProcess) => FlowIDEConnection =
        defaultIDEConnectionFactory,
  ) {
  }
}

interface F {
  ideConnectionFactoryLongLongLong: hook (child_process$ChildProcess) => FlowIDEConnection
}

type ExtractType = hook <A>(B<C>) => D

type T3 = ?(hook () => A);

type T4 = ?(hook () => A) | B;

type T5 = ?hook () => A | B;

type T6 = (?hook () => A) | B;

// Attached comment
type T = hook (
  /**
   * Commet block
   */
  bar: string, // Trailing comment

  // preceding comment
  baz: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
) => void;

type T = hook (
  ...props: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
) => SomeComponent;

type T = hook (
  ...props: $ReadOnly<{k: string, reallllllllllllllllllllllllllllllllllllllyLong: string}>
  // Trailing comment
) => SomeComponent;
    `;
    const output = await prettier.format(code, getOptions());
    expect(output).toMatchInlineSnapshot(`
      "type T = hook () => void;

      type T = hook (bar) => SomeComponent;

      type T = hook () => React.Element<
        typeof SomeComponentLonnnnnnnnnnnnnnnnnnnnnnnnnnnnng,
      >;

      type T = hook <T>() => void;

      type T = hook <
        T,
      >() => Array<Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo>;

      type T = hook <
        T: Fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo,
      >() => void;

      type T = hook (bar: string) => string;

      type T = hook (bar?: string) => string;

      type T = hook (data: string) => string;

      type T = hook (...restProps: $ReadOnly<{k: string}>) => {k: string};

      type T = hook (...$ReadOnly<{k: string}>) => {k: string};

      type T = hook (bar: string, baz: $ReadOnly<{k: string}>) => void;

      type T = hook (
        bar: string,
        baz: $ReadOnly<{k: string}>,
        realllllllllllllllllllyLong: string,
      ) => void;

      type Banana = {
        useFoo: hook (string) => boolean,
      };

      interface F2 {
        m: hook (string) => number;
      }

      type f3 = hook (...arg) => void;

      type f4 = hook (/* comment */ arg) => void;

      type f5 = hook (arg /* comment */) => void;

      type f6 = hook (?arg) => void;

      class Y {
        constructor(
          ideConnectionFactory: hook (
            child_process$ChildProcess,
          ) => FlowIDEConnection = defaultIDEConnectionFactory,
        ) {}
      }

      interface F {
        ideConnectionFactoryLongLongLong: hook (
          child_process$ChildProcess,
        ) => FlowIDEConnection;
      }

      type ExtractType = hook <A>(B<C>) => D;

      type T3 = ?hook () => A;

      type T4 = ?hook () => A | B;

      type T5 = ?hook () => A | B;

      type T6 = ?hook () => A | B;

      // Attached comment
      type T = hook (
        /**
         * Commet block
         */
        bar: string, // Trailing comment

        // preceding comment
        baz: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>,
      ) => // Trailing comment
      void;

      type T = hook (
        ...props: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>
      ) => // Trailing comment
      SomeComponent;

      type T = hook (
        ...props: $ReadOnly<{
          k: string,
          reallllllllllllllllllllllllllllllllllllllyLong: string,
        }>
      ) => // Trailing comment
      SomeComponent;
      "
    `);
  });
});
