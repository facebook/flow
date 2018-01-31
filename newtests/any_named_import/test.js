/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */


import {suite, test} from '../../packages/flow-dev-tools/src/test/Tester';

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
                ^^^ Cannot cast \`Any\` to number because module \`any\` [1] is incompatible with number [2].
            References:
              5: (Any: number);
                  ^^^ [1]: module \`any\`
              5: (Any: number);
                       ^^^^^^ [2]: number
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
                ^^^ Cannot cast \`obj\` to string because object type [1] is incompatible with string [2].
            References:
              5: (obj: string);
                  ^^^ [1]: object type
              5: (obj: string);
                       ^^^^^^ [2]: string
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
                ^^^ Cannot cast \`obj\` to string because object type [1] is incompatible with string [2].
            References:
              5: (obj: string);
                  ^^^ [1]: object type
              5: (obj: string);
                       ^^^^^^ [2]: string
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
                ^^^ Cannot cast \`obj\` to number because module \`object\` [1] is incompatible with number [2].
            References:
              5: (obj: number);
                  ^^^ [1]: module \`object\`
              5: (obj: number);
                       ^^^^^^ [2]: number
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
