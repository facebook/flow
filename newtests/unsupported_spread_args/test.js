/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

/* This test suite documents a bunch of places where using spread arguments
 * doesn't work, either intentionally or due to us being lazy */
export default suite(({addFile, addFiles, addCode}) => [
  test('idx', [
    addCode('declare var idx: $Facebookism$Idx;\n'),
    addCode('idx(...arr, obj => obj.foo)')
      .newErrors(
        `
          test.js:8
            8: idx(...arr, obj => obj.foo)
                                      ^^^ property \`foo\`. Property cannot be accessed on
            8: idx(...arr, obj => obj.foo)
                                  ^^^ rest array of spread operand
        `,
      ),
    addCode('idx({}, ...arr)')
      .newErrors(
        `
          test.js:10
           10: idx({}, ...arr)
               ^^^^^^^^^^^^^^^ function call. Function cannot be called on
           10: idx({}, ...arr)
                          ^^^ rest array of spread operand
        `,
      ),
    addCode('idx(...arr, ...arr)')
      .newErrors(
        `
          test.js:12
           12: idx(...arr, ...arr)
               ^^^^^^^^^^^^^^^^^^^ function call. Function cannot be called on
           12: idx(...arr, ...arr)
                              ^^^ rest array of spread operand
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
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ call of method \`createElement\`. Expected React component instead of
            7: React.createElement(...arr, {})
                                      ^^^ rest array of spread operand
        `,
      ),
    addCode('React.createElement(({}: any), ...arr)')
      .noNewErrors(),
    addCode('React.createElement(...arr, ...arr)')
      .newErrors(
        `
          test.js:11
           11: React.createElement(...arr, ...arr)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ call of method \`createElement\`. Expected React component instead of
           11: React.createElement(...arr, ...arr)
                                      ^^^ rest array of spread operand
        `,
      ),
  ]),

  test('fun.call()', [
    addCode('(function () { return this.bar; }).call(...arr);')
      .newErrors(
        `
          test.js:5
            5: (function () { return this.bar; }).call(...arr);
                                          ^^^ property \`bar\`. Property cannot be accessed on
            5: (function () { return this.bar; }).call(...arr);
                                     ^^^^ rest array of spread operand
        `,
      ),
  ]),

  test('fun.apply()', [
    addCode('(function () { return this.bar; }).apply(...arr);')
      .newErrors(
        `
          test.js:5
            5: (function () { return this.bar; }).apply(...arr);
                                          ^^^ property \`bar\`. Property cannot be accessed on
            5: (function () { return this.bar; }).apply(...arr);
                                     ^^^^ rest array of spread operand
        `,
      ),
    addCode('(function () { return this.bar; }).apply(({}: any), ...arr);')
      .newErrors(
        `
          test.js:7
            7: (function () { return this.bar; }).apply(({}: any), ...arr);
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ call of method \`apply\`. Expected array of arguments instead of
            7: (function () { return this.bar; }).apply(({}: any), ...arr);
                                                                      ^^^ rest array of spread operand
        `,
      ),
    addCode('(function () { return this.bar; }).apply(...arr, ...arr);')
      .newErrors(
        `
          test.js:9
            9: (function () { return this.bar; }).apply(...arr, ...arr);
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ call of method \`apply\`. Expected array of arguments instead of
            9: (function () { return this.bar; }).apply(...arr, ...arr);
                                                                   ^^^ rest array of spread operand
        `,
      ),
  ]),
  test('Object.getPrototypeOf()', [
    addCode('Object.getPrototypeOf(...arr)')
      .newErrors(
        `
          test.js:5
            5: Object.getPrototypeOf(...arr)
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ call of method \`getPrototypeOf\`. Property cannot be accessed on
            5: Object.getPrototypeOf(...arr)
                                        ^^^ rest array of spread operand
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
          test.js:9
            9:       o1.x;
                        ^ property \`x\`. Property cannot be accessed on
            9:       o1.x;
                     ^^ rest array of spread operand
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
               ^^^^^^^^^^^^^^^^^^^^^^^^^ call of method \`assign\`. Expected object instead of
           17: Object.assign({}, ...[1])
                                     ^ number
        `,
      )
      .because('But this is an error since the array contains non-objects'),
  ]),
  test('mergeInto', [
    addCode('declare var mergeInto: $Facebookism$MergeInto'),
    addCode('mergeInto(...arr, ...arr)')
      .newErrors(
        `
          test.js:7
            7: mergeInto(...arr, ...arr)
               ^^^^^^^^^^^^^^^^^^^^^^^^^ function call. Expected object instead of
            3: const arr = [1,2,3];
                            ^ number

          test.js:7
            7: mergeInto(...arr, ...arr)
               ^^^^^^^^^^^^^^^^^^^^^^^^^ function call. Expected object instead of
            3: const arr = [1,2,3];
                              ^ number

          test.js:7
            7: mergeInto(...arr, ...arr)
               ^^^^^^^^^^^^^^^^^^^^^^^^^ function call. Expected object instead of
            3: const arr = [1,2,3];
                                ^ number
        `,
      )
      .because('First arg cant be a spread, second can'),
  ]),
]).beforeEach((({addCode}) => [ addCode('const arr = [1,2,3];') ]));
