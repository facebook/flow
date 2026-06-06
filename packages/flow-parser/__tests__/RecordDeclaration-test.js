/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

import {printForSnapshot} from '../__test_utils__/parse';

const transform = (src: string) => printForSnapshot(src, {babel: true});

function run(declaration: string, testExp: string): mixed {
  const f: $FlowFixMe = new Function(`${declaration}\nreturn ${testExp};`);
  return f();
}

describe('RecordDeclaration', () => {
  test('empty', async () => {
    const code = `
      record R {}
    `;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "class R {
       constructor({}) {}

     }"
    `);

    expect(run(output, `Object.keys(new R({}))`)).toEqual([]);
  });

  test('basic', async () => {
    const code = `
      record R {
        foo: number,
      }
    `;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "class R {
       constructor({
         foo
       }) {
         this.foo = foo;
       }

     }"
    `);

    expect(run(output, `new R({foo: 1})`)).toMatchObject({foo: 1});
  });

  test('key as reserved ident or literal', async () => {
    const code = `
      record R {
        default: number,
        1: number,
        'a b': string,
        2: number = 2,
        class: number = 3,
      }
    `;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "class R {
       constructor({
         default: $$gen$r0,
         1: $$gen$r1,
         'a b': $$gen$r2,
         2: $$gen$r3 = 2,
         class: $$gen$r4 = 3
       }) {
         this.default = $$gen$r0;
         this[1] = $$gen$r1;
         this['a b'] = $$gen$r2;
         this[2] = $$gen$r3;
         this.class = $$gen$r4;
       }

     }"
    `);

    expect(run(output, `new R({default: 0, 1: 1, 'a b': 's'})`)).toMatchObject({
      default: 0,
      1: 1,
      'a b': 's',
      2: 2,
      class: 3,
    });
  });

  test('defaulted props', async () => {
    const code = `
      record R {
        foo: number = 0,
      }
    `;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "class R {
       constructor({
         foo = 0
       }) {
         this.foo = foo;
       }

     }"
    `);

    expect(run(output, `new R({foo: 1}).foo`)).toBe(1);
    expect(run(output, `new R({}).foo`)).toBe(0);
    expect(run(output, `new R({foo: undefined})`)).toMatchObject({foo: 0});
  });

  test('methods', async () => {
    const code = `
      record R {
        foo: number,

        equals(other) {
          return this.foo === other.foo;
        }
      }
    `;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "class R {
       constructor({
         foo
       }) {
         this.foo = foo;
       }

       equals(other) {
         return this.foo === other.foo;
       }

     }"
    `);

    expect(run(output, `(new R({foo: 1})).equals(new R({foo: 1}))`)).toBe(true);
  });

  test('static prop', async () => {
    const code = `
      record R {
        static bar: number = 1,
      }
    `;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "class R {
       constructor({}) {}

       static bar = 1;
     }"
    `);

    expect(run(output, `R.bar`)).toBe(1);
  });

  test('static method', async () => {
    const code = `
      record R {
        static f() {
          return 1;
        }
      }
    `;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "class R {
       constructor({}) {}

       static f() {
         return 1;
       }

     }"
    `);

    expect(run(output, `R.f()`)).toBe(1);
  });

  test('all', async () => {
    const code = `
      record R<T> implements Iface {
        a: number,
        b: number = Math.PI * 2,
        c: string,

        static s: boolean = true,

        f() {
          return [this.a, this.b, this.c];
        }

        static g() {
          return R.s;
        }
      }
    `;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "class R {
       constructor({
         a,
         b = Math.PI * 2,
         c
       }) {
         this.a = a;
         this.b = b;
         this.c = c;
       }

       f() {
         return [this.a, this.b, this.c];
       }

       static s = true;

       static g() {
         return R.s;
       }

     }"
    `);

    expect(run(output, `new R({a: 1, c: 's'}).f()`)).toEqual([
      1,
      Math.PI * 2,
      's',
    ]);
    expect(run(output, `R.g()`)).toBe(true);
  });
});
