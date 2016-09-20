/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('You can do any named import for an any module', [
    addFile('flow-typed/lib.js')
      .addCode('import {x, y} from "any";')
      .addCode('(x: string);')
      .addCode('(y: number);')
      .noNewErrors()
      .because('x and y both have type any'),
  ]),
  test('The default import for the any module should be any', [
    addFile('flow-typed/lib.js')
      .addCode('import Any from "any";')
      .addCode('(Any: string);')
      .noNewErrors()
      .because('Any should have the type any'),
  ]),
  test('The cjs require for the any module should be any', [
    addFile('flow-typed/lib.js')
      .addCode('const Any = require("any");')
      .addCode('(Any: string);')
      .noNewErrors()
      .because('Any should have the type any'),
  ]),
  test('The namespace import for the any module should be any', [
    addFile('flow-typed/lib.js')
      .addCode('import * as Any from "any";')
      .addCode('(Any: number);')
      .addCode('(Any: {[key: string]: any});')
      .addCode('(Any.foo: string)')
      .newErrors(
        `
          test.js:5
            5: (Any: number);
                ^^^ exports of "any". This type is incompatible with
            5: (Any: number);
                     ^^^^^^ number
        `,
      )
      .because('Any should be { [key: string]: any }'),
  ]),
  test('You can do any named import for an Object module', [
    addFile('flow-typed/lib.js')
      .addCode('import {x, y} from "object";')
      .addCode('(x: string);')
      .addCode('(y: number);')
      .noNewErrors()
      .because('x and y both have type any'),
  ]),
  test('The default import for the object module should be Object', [
    addFile('flow-typed/lib.js')
      .addCode('import obj from "object";')
      .addCode('(obj: string);')
      .newErrors(
        `
          test.js:5
            5: (obj: string);
                ^^^ object type. This type is incompatible with
            5: (obj: string);
                     ^^^^^^ string
        `,
      )
      .because('obj should have the type Object'),
  ]),
  test('The cjs require for the object module should be Object', [
    addFile('flow-typed/lib.js')
      .addCode('const obj = require("object");')
      .addCode('(obj: string);')
      .newErrors(
        `
          test.js:5
            5: (obj: string);
                ^^^ object type. This type is incompatible with
            5: (obj: string);
                     ^^^^^^ string
        `,
      )
      .because('obj should have the type Object'),
  ]),
  test('The namespace import for the object module should be object', [
    addFile('flow-typed/lib.js')
      .addCode('import * as obj from "object";')
      .addCode('(obj: number);')
      .addCode('(obj: {[key: string]: any});')
      .addCode('(obj.foo: string)')
      .newErrors(
        `
          test.js:5
            5: (obj: number);
                ^^^ exports of "object". This type is incompatible with
            5: (obj: number);
                     ^^^^^^ number
        `,
      )
      .because('obj should be { [key: string]: any }'),
  ]),
  test('Other types still give an error', [
    addFile('flow-typed/lib.js')
      .addCode('import {x, y} from "string";')
      .addCode('(x: string);')
      .newErrors(
        `
          test.js:3
            3: import {x, y} from "string";
                       ^ Named import from module \`string\`. This module only has a default export. Did you mean \`import x from ...\`?

          test.js:3
            3: import {x, y} from "string";
                          ^ Named import from module \`string\`. This module only has a default export. Did you mean \`import y from ...\`?
        `,
      ),
  ]),
]);
