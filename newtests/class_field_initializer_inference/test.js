/*
 * @flow
 */


import type Suite from "flow-dev-tools/src/test/Suite.js";
import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default (suite(({addFile, addFiles, addCode}) => [
  test('Uninitialized instance fields require annotation', [
    addCode('export class Foo { a; }')
      .newErrors(
        `
          test.js:3
            3: export class Foo { a; }
                                  ^^ Missing type annotation for property \`a\`. [missing-annot]
        `,
      )
  ]),

  test('Annotated instance fields dont require annotation', [
    addCode('export class Foo { a: number; }')
      .noNewErrors()
  ]),

  test('Initialized instance fields infer type from init value', [
    addCode('export class Foo { a = 42; }')
      .noNewErrors()
  ]),

  test('Initialized instance fields require annotation within init values', [
    addCode('export class Foo { a = (p) => 42; }')
      .newErrors(
        `
          test.js:3
            3: export class Foo { a = (p) => 42; }
                                       ^ Missing type annotation for \`p\`. [missing-annot]
        `,
      ),
    addCode('export class Bar { a = (p: number) => 42; }')
      .noNewErrors()
  ]),

]): Suite);
