/*
 * @flow
 */


import type Suite from "flow-dev-tools/src/test/Suite.js";
import {suite, test} from 'flow-dev-tools/src/test/Tester';

export default (suite(({addFile, addFiles, addCode}) => [
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
                ^^^ Cannot cast \`Any\` to number because module \`any\` [1] is incompatible with number [2]. [incompatible-cast]
            References:
              3: import * as Any from "any";
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
              5: (Any: number);
                       ^^^^^^ [2]
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
      .noNewErrors()
      .because('obj should have the type Object'),
  ]),
  test('The cjs require for the object module should be Object', [
    addFile('flow-typed/lib.js')
      .addCode('const obj = require("object");')
      .addCode('(obj: string);')
      .noNewErrors()
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
                ^^^ Cannot cast \`obj\` to number because module \`object\` [1] is incompatible with number [2]. [incompatible-cast]
            References:
              3: import * as obj from "object";
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
              5: (obj: number);
                       ^^^^^^ [2]
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
                       ^ Cannot import \`x\` because there is no \`x\` export in \`string\`. Did you mean \`import x from "..."\`? [missing-export]

          test.js:3
            3: import {x, y} from "string";
                          ^ Cannot import \`y\` because there is no \`y\` export in \`string\`. Did you mean \`import y from "..."\`? [missing-export]
        `,
      ),
  ]),
]): Suite);
