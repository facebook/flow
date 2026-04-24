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

describe(`'readonly' variance annotation`, () => {
  test('class property', async () => {
    expect(
      await format(`
        class ReadonlyRoute {
            static   readonly   param: T;
        }
      `),
    ).toMatchInlineSnapshot(`
      "class ReadonlyRoute {
        static readonly param: T;
      }
      "
    `);
  });

  test('object type property', async () => {
    expect(
      await format(`
        type ReadonlyObj = {
            readonly   foo:   string,
          readonly [string]:   unknown
        };
      `),
    ).toMatchInlineSnapshot(`
      "type ReadonlyObj = {
        readonly foo: string,
        readonly [string]: unknown,
      };
      "
    `);
  });

  test('tuple label', async () => {
    expect(
      await format(`
        type ReadonlyTuple = [  readonly   label:   number  ];
      `),
    ).toMatchInlineSnapshot(`
      "type ReadonlyTuple = [readonly label: number];
      "
    `);
  });

  test('interface', async () => {
    expect(
      await format(`
        type ReadonlyInterface = interface {
              readonly   prop:   string
        };
      `),
    ).toMatchInlineSnapshot(`
      "type ReadonlyInterface = interface {
        readonly prop: string,
      };
      "
    `);
  });

  // The `+` variance sigil accepts reserved words as property names, e.g.
  // `{+with: string}` parses fine. The `readonly` modifier should behave
  // the same way: with one token of lookahead the parser can disambiguate
  // `readonly <name> :` (variance modifier) from `readonly :` / `readonly ?`
  // (property literally named `readonly`). Today these inputs throw
  // `SyntaxError: ':' or '?' expected in property type annotation`, which
  // breaks codemods that mechanically rewrite `+x` to `readonly x`.
  describe('reserved-word property names', () => {
    // $FlowFixMe[prop-missing]
    test.failing('with', async () => {
      // eslint-disable-next-line jest/no-standalone-expect
      expect(
        await format(`
          type T = {
            readonly with: (start?: Position, end?: Position) => Range,
          };
        `),
      ).toMatchInlineSnapshot();
    });

    // $FlowFixMe[prop-missing]
    test.failing('enum', async () => {
      // eslint-disable-next-line jest/no-standalone-expect
      expect(
        await format(`
          type T = {
            readonly enum: {+[string]: string},
          };
        `),
      ).toMatchInlineSnapshot();
    });

    // $FlowFixMe[prop-missing]
    test.failing('default (optional)', async () => {
      // eslint-disable-next-line jest/no-standalone-expect
      expect(
        await format(`
          type T<T> = {
            readonly default?: ?T,
          };
        `),
      ).toMatchInlineSnapshot();
    });

    // $FlowFixMe[prop-missing]
    test.failing('new (inline object type)', async () => {
      // eslint-disable-next-line jest/no-standalone-expect
      expect(
        await format(`
          declare class C {
            constructor(value: {readonly old: string, readonly new: string, ...}): void;
          }
        `),
      ).toMatchInlineSnapshot();
    });
  });
});
