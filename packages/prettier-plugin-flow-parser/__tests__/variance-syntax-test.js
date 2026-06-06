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

describe(`'writeonly' variance annotation`, () => {
  test('class property', async () => {
    expect(
      await format(`
        class Foo {
            static   writeonly   param: T;
        }
      `),
    ).toMatchInlineSnapshot(`
"class Foo {
  static writeonly param: T;
}
"
`);
  });

  test('object type property', async () => {
    expect(
      await format(`
        type Obj = {
            writeonly   foo:   string,
          writeonly [string]:   unknown
        };
      `),
    ).toMatchInlineSnapshot(`
"type Obj = {
  writeonly foo: string,
  writeonly [string]: unknown,
};
"
`);
  });

  test('tuple label', async () => {
    expect(
      await format(`
        type WriteonlyTuple = [  writeonly   label:   number  ];
      `),
    ).toMatchInlineSnapshot(`
"type WriteonlyTuple = [writeonly label: number];
"
`);
  });

  test('interface', async () => {
    expect(
      await format(`
        type WriteonlyInterface = interface {
              writeonly   prop:   string
        };
      `),
    ).toMatchInlineSnapshot(`
"type WriteonlyInterface = interface {
  writeonly prop: string,
};
"
`);
  });
});

describe(`'in' variance annotation`, () => {
  test('type parameter', async () => {
    expect(
      await format(`
        type Foo<  in   T  > = T;
      `),
    ).toMatchInlineSnapshot(`
"type Foo<in T> = T;
"
`);
  });

  test('class type parameter', async () => {
    expect(
      await format(`
        class Foo<  in   T  > {}
      `),
    ).toMatchInlineSnapshot(`
"class Foo<in T> {}
"
`);
  });

  test('interface type parameter', async () => {
    expect(
      await format(`
        interface Foo<  in   T  > {}
      `),
    ).toMatchInlineSnapshot(`
"interface Foo<in T> {}
"
`);
  });
});

describe(`'out' variance annotation`, () => {
  test('type parameter', async () => {
    expect(
      await format(`
        type Foo<  out   T  > = T;
      `),
    ).toMatchInlineSnapshot(`
"type Foo<out T> = T;
"
`);
  });

  test('class type parameter', async () => {
    expect(
      await format(`
        class Foo<  out   T  > {}
      `),
    ).toMatchInlineSnapshot(`
"class Foo<out T> {}
"
`);
  });

  test('interface type parameter', async () => {
    expect(
      await format(`
        interface Foo<  out   T  > {}
      `),
    ).toMatchInlineSnapshot(`
"interface Foo<out T> {}
"
`);
  });
});
