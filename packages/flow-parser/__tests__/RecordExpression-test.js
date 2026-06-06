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

function run(declaration: string, expression: string): mixed {
  const f: $FlowFixMe = new Function(`${declaration}\nreturn ${expression};`);
  return f();
}

describe('RecordExpression', () => {
  test('empty', async () => {
    const code = `R {}`;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`"new R({});"`);

    const declaration = await transform(`record R {}`);
    expect(run(declaration, output)).toMatchObject({});
  });

  test('basic', async () => {
    const code = `R {a: 1, b: true}`;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "new R({
       a: 1,
       b: true
     });"
    `);

    const declaration = await transform(`record R {a: number, b: boolean}`);
    expect(run(declaration, output)).toMatchObject({a: 1, b: true});
  });

  test('spread', async () => {
    const code = `R {...x, a: 1}`;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "new R({ ...x,
       a: 1
     });"
    `);

    const declaration = await transform(
      `record R {a: number, b: boolean}; const x = {b: true};`,
    );
    expect(run(declaration, output)).toMatchObject({a: 1, b: true});
  });

  test('nested', async () => {
    const code = `R {a: 1, b: S {c: 2}}`;
    const output = await transform(code);
    expect(output).toMatchInlineSnapshot(`
     "new R({
       a: 1,
       b: new S({
         c: 2
       })
     });"
    `);

    const declaration = await transform(
      `record S {c: number} record R {a: number, b: S}`,
    );
    expect(run(declaration, output)).toMatchObject({a: 1, b: {c: 2}});
  });
});
