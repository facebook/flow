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
                                  ^^ Cannot build a typed interface for this module. You should annotate the exports of this module with types. Missing type annotation at property \`a\`: [signature-verification-failure]
        `,
      )
  ]),

  test('Annotated instance fields dont require annotation', [
    addCode('export class Foo { a: number; }')
      .noNewErrors()
  ]),

  test('Initialized instance fields infer type from init value', [
    addCode('export class Foo { a: number = 42; }')
      .noNewErrors()
  ]),

  test('Initialized instance fields require annotation within init values', [
    addCode('export class Foo { a = (p) => 42; }')
      .newErrors(
        `
          test.js:3
            3: export class Foo { a = (p) => 42; }
            ^^^^^^^^^^^^^^ Cannot build a typed interface for this module. You should annotate the exports of this module with types. Missing type annotation at property \`a\`: [signature-verification-failure]
        `,
      ),
    addCode('export class Bar { a: (p: number) => number = (p) => 42; }')
      .noNewErrors()
  ]),

]): Suite);
