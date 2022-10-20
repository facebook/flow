/*
 * @flow
 */


import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

/* This test suite documents a bunch of places where using spread arguments
 * doesn't work, either intentionally or due to us being lazy */
module.exports = (suite(({addFile, addFiles, addCode}) => [
  test('React.createElement', [
    addCode('const React = require("react");'),
    addCode('React.createElement(...arr, {})')
      .newErrors(
        `
					test.js:3          3: const arr = [1,2,3];
					                    ^ Cannot instantiate React ref because number [1] is incompatible with string [2] in type argument \`ElementType\`. [incompatible-type-arg]
					  References:
					    3: const arr = [1,2,3];
					                    ^ [1]
					  162:   | string
					           ^^^^^^ [2]. See lib: [LIB] react.js:162
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
    addCode('(function (this: any, ...args: any) { return this.bar; }).call(...arr);')
      .newErrors(
        `
          test.js:5
            5: (function (this: any, ...args: any) { return this.bar; }).call(...arr);
                                                                                 ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
  ]),

  test('fun.apply()', [
    addCode('(function (this: any) { return this.bar; }).apply(...arr);')
      .newErrors(
        `
          test.js:5
            5: (function (this: any) { return this.bar; }).apply(...arr);
                                                                    ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
    addCode('(function (this: any) { return this.bar; }).apply(({}: any), ...arr);')
      .newErrors(
        `
          test.js:7
            7: (function (this: any) { return this.bar; }).apply(({}: any), ...arr);
                                                                               ^^^ A spread argument is unsupported here. [unsupported-syntax]
        `,
      ),
    addCode('(function (this: any) { return this.bar; }).apply(...arr, ...arr);')
      .newErrors(
        `
          test.js:9
            9: (function (this: any) { return this.bar; }).apply(...arr, ...arr);
                                                                    ^^^ A spread argument is unsupported here. [unsupported-syntax]

          test.js:9
            9: (function (this: any) { return this.bar; }).apply(...arr, ...arr);
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
      const o2 = Object.assign(({}: {x?: string, y?: number}), ...objArr);
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
