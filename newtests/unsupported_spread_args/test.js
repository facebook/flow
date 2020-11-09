/*
 * @flow
 */


import type Suite from "flow-dev-tools/src/test/Suite.js";
import {suite, test} from 'flow-dev-tools/src/test/Tester';

/* This test suite documents a bunch of places where using spread arguments
 * doesn't work, either intentionally or due to us being lazy */
export default (suite(({addFile, addFiles, addCode}) => [
  test('idx', [
    addCode('declare var idx: $Facebookism$Idx;\n'),
    addCode('idx(...arr, obj => obj.foo)')
      .newErrors(
        `
          test.js:8
            8: idx(...arr, obj => obj.foo)
                      ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
    addCode('idx({}, ...arr)')
      .newErrors(
        `
          test.js:10
           10: idx({}, ...arr)
                          ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
    addCode('idx(...arr, ...arr)')
      .newErrors(
        `
          test.js:12
           12: idx(...arr, ...arr)
                      ^^^ A spread argument is unsupported here. [unsupported-syntax]

          test.js:12
           12: idx(...arr, ...arr)
                              ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
  ]),

  test('React.createElement', [
    addCode('const React = require("react");'),
    addCode('React.createElement(...arr, {})')
      .newErrors(
        `
          test.js:7
            7: React.createElement(...arr, {})
                     ^^^^^^^^^^^^^ Cannot call \`React.createElement\` because number [1] is not a React component. [not-a-component]
            References:
              3: const arr = [1,2,3];
                              ^ [1]

          test.js:7
            7: React.createElement(...arr, {})
                     ^^^^^^^^^^^^^ Cannot call \`React.createElement\` because number [1] is not an object. [not-an-object]
            References:
              3: const arr = [1,2,3];
                                ^ [1]
        `,
      ),
    addCode('React.createElement(({}: any), ...arr)')
      .newErrors(
        `
          test.js:9
            9: React.createElement(({}: any), ...arr)
                     ^^^^^^^^^^^^^ Cannot call \`React.createElement\` because number [1] is not an object. [not-an-object]
            References:
              3: const arr = [1,2,3];
                              ^ [1]
        `,
      ),
    addCode('React.createElement(...arr, ...arr)')
      .newErrors(
        `
          test.js:11
           11: React.createElement(...arr, ...arr)
                     ^^^^^^^^^^^^^ Cannot call \`React.createElement\` because number [1] is not a React component. [not-a-component]
            References:
              3: const arr = [1,2,3];
                              ^ [1]

          test.js:11
           11: React.createElement(...arr, ...arr)
                     ^^^^^^^^^^^^^ Cannot call \`React.createElement\` because number [1] is not an object. [not-an-object]
            References:
              3: const arr = [1,2,3];
                                ^ [1]
        `,
      ),
  ]),

  test('fun.call()', [
    addCode('(function (...args) { return this.bar; }).call(...arr);')
      .newErrors(
        `
          test.js:5
            5: (function (...args) { return this.bar; }).call(...arr);
                                                                 ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
  ]),

  test('fun.apply()', [
    addCode('(function () { return this.bar; }).apply(...arr);')
      .newErrors(
        `
          test.js:5
            5: (function () { return this.bar; }).apply(...arr);
                                                           ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
    addCode('(function () { return this.bar; }).apply(({}: any), ...arr);')
      .newErrors(
        `
          test.js:7
            7: (function () { return this.bar; }).apply(({}: any), ...arr);
                                                                      ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
    addCode('(function () { return this.bar; }).apply(...arr, ...arr);')
      .newErrors(
        `
          test.js:9
            9: (function () { return this.bar; }).apply(...arr, ...arr);
                                                           ^^^ A spread argument is unsupported here. [unsupported-syntax]

          test.js:9
            9: (function () { return this.bar; }).apply(...arr, ...arr);
                                                                   ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
  ]),
  test('Object.getPrototypeOf()', [
    addCode('Object.getPrototypeOf(...arr)')
      .newErrors(
        `
          test.js:5
            5: Object.getPrototypeOf(...arr)
                                        ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
  ]),
  test('Object.assign()', [
    addCode('const objArr = [ {x: string}, {y: number}]'),
    addCode(`
      const o1 = Object.assign(...objArr);
      o1.x;
    `)
      .newErrors(
        `
          test.js:8
            8:       const o1 = Object.assign(...objArr);
                                                 ^^^^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
    addCode(`
      const o2 = Object.assign({}, ...objArr);
      o2.x;
    `)
      .noNewErrors()
      .because('This is actually fine, we just use array element type'),
    addCode('Object.assign({}, ...[1])')
      .newErrors(
        `
          test.js:17
           17: Object.assign({}, ...[1])
               ^^^^^^^^^^^^^^^^^^^^^^^^^ Incorrect arguments passed to call of method \`assign\` because number [1] is not an object. [not-an-object]
            References:
             17: Object.assign({}, ...[1])
                                       ^ [1]
        `,
      )
      .because('But this is an error since the array contains non-objects'),
  ]),
]).beforeEach((({addCode}) => [ addCode('const arr = [1,2,3];') ])): Suite);
