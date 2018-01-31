/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */

import {suite, test} from '../../packages/flow-dev-tools/src/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('derived object reads must be compatible with prototype writes', [
    addCode(`
      var proto = {};
      var o = Object.create(proto);
    `).noNewErrors(),

    // read installs a shadow property on `proto`
    addCode(`(o.p: string);`).noNewErrors(),

    addCode(`proto.p = 0;`).
      newErrors(
       `
         test.js:8
           8: (o.p: string);
               ^^^ Cannot cast property \`p\` to string because number [1] is incompatible with string [2].
           References:
             8: (o.p: string);
                 ^^^ [1]: number
             8: (o.p: string);
                      ^^^^^^ [2]: string
       `,
     ),
  ]),

  test('derived object reads are independent until prototype is constrained', [
    addCode(`
      var proto = {};
      var a = Object.create(proto);
      var b = Object.create(proto);
      (a.p: string);
      (b.p: number);
    `).noNewErrors(),

    addCode(`proto.p = true;`)
      .newErrors(
        `
          test.js:7
            7:       (a.p: string);
                      ^^^ Cannot cast property \`p\` to string because boolean [1] is incompatible with string [2].
            References:
              7:       (a.p: string);
                        ^^^ [1]: boolean
              7:       (a.p: string);
                             ^^^^^^ [2]: string

          test.js:8
            8:       (b.p: number);
                      ^^^ Cannot cast property \`p\` to number because boolean [1] is incompatible with number [2].
            References:
              8:       (b.p: number);
                        ^^^ [1]: boolean
              8:       (b.p: number);
                             ^^^^^^ [2]: number
        `,
      ),
  ]),

  test('derived object writes are independent until prototype is constrained', [
    addCode(`
      var proto = {};
      var a = Object.create(proto);
      var b = Object.create(proto);
      a.p = 0;
      b.p = "";
      console.log("havoc definite refinements");
    `).noNewErrors(),

    // *not* string ~> void
    addCode(`(a.p: void);`)
      .newErrors(
        `
          test.js:12
           12: (a.p: void);
                ^^^ Cannot cast property \`p\` to undefined because number [1] is incompatible with undefined [2].
            References:
             12: (a.p: void);
                  ^^^ [1]: number
             12: (a.p: void);
                       ^^^^ [2]: undefined
        `,
      ),

    // *not* number ~> void
    addCode(`(b.p: void);`)
      .newErrors(
        `
          test.js:14
           14: (b.p: void);
                ^^^ Cannot cast property \`p\` to undefined because string [1] is incompatible with undefined [2].
            References:
             14: (b.p: void);
                  ^^^ [1]: string
             14: (b.p: void);
                       ^^^^ [2]: undefined
        `,
      ),

    // TODO: This should add more errors
    addCode(`proto.p = true;`)
      .newErrors(
        `
          test.js:12
           12: (a.p: void);
                ^^^ Cannot cast property \`p\` to undefined because boolean [1] is incompatible with undefined [2].
            References:
             12: (a.p: void);
                  ^^^ [1]: boolean
             12: (a.p: void);
                       ^^^^ [2]: undefined

          test.js:12
           12: (a.p: void);
                ^^^ Cannot cast property \`p\` to undefined because string [1] is incompatible with undefined [2].
            References:
             12: (a.p: void);
                  ^^^ [1]: string
             12: (a.p: void);
                       ^^^^ [2]: undefined

          test.js:14
           14: (b.p: void);
                ^^^ Cannot cast property \`p\` to undefined because boolean [1] is incompatible with undefined [2].
            References:
             14: (b.p: void);
                  ^^^ [1]: boolean
             14: (b.p: void);
                       ^^^^ [2]: undefined

          test.js:14
           14: (b.p: void);
                ^^^ Cannot cast property \`p\` to undefined because number [1] is incompatible with undefined [2].
            References:
             14: (b.p: void);
                  ^^^ [1]: number
             14: (b.p: void);
                       ^^^^ [2]: undefined
        `,
      ),
  ]),

  test('derived object subtyping -- write before read', [
    addCode(`
      var proto = {};
      proto.p = 0;
    `).noNewErrors(),

    addCode(`var o: {p: string} = Object.create(proto);`)
      .newErrors(
        `
          test.js:8
            8: var o: {p: string} = Object.create(proto);
                                    ^^^^^^^^^^^^^^^^^^^^ Object.create. This type is incompatible with
            8: var o: {p: string} = Object.create(proto);
                      ^^^^^^^^^^^ object type
            Property \`p\` is incompatible:
                5:       proto.p = 0;
                                   ^ number. This type is incompatible with
                8: var o: {p: string} = Object.create(proto);
                              ^^^^^^ string
        `,
      ),
  ]),

  test('derived object subtyping -- read before write', [
    addCode(`
      var proto = {};
    `).noNewErrors(),

    /* NOTE: Shadow reads for ObjT -> ObjT are currently strict, which is
       inconsistent with GetPropT/MethodT. It would be confusing if this
       didn't error, though: var o: { p: string } = {} */
    addCode(`var o: {p: string} = Object.create(proto);`)
      .newErrors(
        `
          test.js:7
            7: var o: {p: string} = Object.create(proto);
                                    ^^^^^^^^^^^^^^^^^^^^ Object.create. This type is incompatible with
            7: var o: {p: string} = Object.create(proto);
                      ^^^^^^^^^^^ object type
            Property \`p\` is incompatible:
                7: var o: {p: string} = Object.create(proto);
                          ^^^^^^^^^^^ property \`p\`. Property not found in
                7: var o: {p: string} = Object.create(proto);
                                        ^^^^^^^^^^^^^^^^^^^^ Object.create
        `,
      ),

    addCode(`proto.p = 0;`)
      .newErrors(
        `
          test.js:9
            9: proto.p = 0;
                         ^ number [1] is incompatible with string [2].
            References:
              9: proto.p = 0;
                           ^ [1]: number
              9: proto.p = 0;
                 ^^^^^^^ [2]: string
        `,
      ),
  ]),

  /* Because shadow operations execute on failure, a builtin or import can cause
     lookups to build up as upper bounds of an tvar. Once the tvar is resolved,
     the various lookups shouldn't clobber each other. */

  test('racy read before write', [
    addFile('proto.js'),
    addCode(`
      import proto from './proto';
      var o = Object.create(proto);
      if (Math.random() < 0.5) {
        (o.p: number);
      } else {
        (o.p: string);
      }
    `),

    addCode(`o.p = true;`)
      .newErrors(
        `
          test.js:7
            7:         (o.p: number);
                        ^^^ Cannot cast property \`p\` to number because boolean [1] is incompatible with number [2].
            References:
              7:         (o.p: number);
                          ^^^ [1]: boolean
              7:         (o.p: number);
                               ^^^^^^ [2]: number

          test.js:9
            9:         (o.p: string);
                        ^^^ Cannot cast property \`p\` to string because boolean [1] is incompatible with string [2].
            References:
              9:         (o.p: string);
                          ^^^ [1]: boolean
              9:         (o.p: string);
                               ^^^^^^ [2]: string
        `,
      ),
  ]),

  test('racy read before write 2', [
    addFile('proto.js'),
    addCode(`
      import proto from './proto';
      var o = Object.create(proto);
      function f() {
        (o.p: void);
      }
    `).noNewErrors(),

    addCode(`
      if (Math.random() < 0.5) {
        o.p = 0;
      } else {
        o.p = "";
      }
    `).newErrors(
        `
          test.js:7
            7:         (o.p: void);
                        ^^^ Cannot cast property \`p\` to undefined because number [1] is incompatible with undefined [2].
            References:
              7:         (o.p: void);
                          ^^^ [1]: number
              7:         (o.p: void);
                               ^^^^ [2]: undefined

          test.js:7
            7:         (o.p: void);
                        ^^^ Cannot cast property \`p\` to undefined because string [1] is incompatible with undefined [2].
            References:
              7:         (o.p: void);
                          ^^^ [1]: string
              7:         (o.p: void);
                               ^^^^ [2]: undefined
        `,
      ),
  ]),

  test('racy read before write 3', [
    addFile('proto.js'),
    addCode(`
      import lib_proto from './proto';
      var proto = Object.create(lib_proto);
      var a = Object.create(proto);
      var b = Object.create(proto);
      if (Math.random() < 0.5) {
        (a.p: number);
      } else {
        (b.p: string);
      }
    `).noNewErrors(),

    addCode(`proto.p = true;`)
      .newErrors(
        `
          test.js:9
            9:         (a.p: number);
                        ^^^ Cannot cast property \`p\` to number because boolean [1] is incompatible with number [2].
            References:
              9:         (a.p: number);
                          ^^^ [1]: boolean
              9:         (a.p: number);
                               ^^^^^^ [2]: number

          test.js:11
           11:         (b.p: string);
                        ^^^ Cannot cast property \`p\` to string because boolean [1] is incompatible with string [2].
            References:
             11:         (b.p: string);
                          ^^^ [1]: boolean
             11:         (b.p: string);
                               ^^^^^^ [2]: string
        `,
      ),
  ]),

  test('racy write before read', [
    addFile('proto.js'),
    addCode(`
      import proto from './proto';
      let o = Object.create(proto);
      if (Math.random() < 0.5) {
        o.p = 0;
      } else {
        o.p = "";
      }
    `).noNewErrors(),

    addCode(`(o.p: void);`)
      .newErrors(
        `
          test.js:13
           13: (o.p: void);
                ^^^ Cannot cast \`o.p\` to undefined because number [1] is incompatible with undefined [2].
            References:
             13: (o.p: void);
                  ^^^ [1]: number
             13: (o.p: void);
                       ^^^^ [2]: undefined

          test.js:13
           13: (o.p: void);
                ^^^ Cannot cast \`o.p\` to undefined because string [1] is incompatible with undefined [2].
            References:
             13: (o.p: void);
                  ^^^ [1]: string
             13: (o.p: void);
                       ^^^^ [2]: undefined
        `,
      ),
  ]),

  test('racy write before read 2', [
    addFile('proto.js'),
    addCode(`
      import proto from './proto';
      var o = Object.create(proto);
      function f() {
        o.p = true;
      }
    `).noNewErrors(),

    addCode(`
      if (Math.random() < 0.5) {
        (o.p: number); // error: void ~> number
      } else {
        (o.p: string); // error: void ~> string
      }
    `).newErrors(
        `
          test.js:13
           13:         (o.p: number); // error: void ~> number
                        ^^^ Cannot cast property \`p\` to number because boolean [1] is incompatible with number [2].
            References:
             13:         (o.p: number); // error: void ~> number
                          ^^^ [1]: boolean
             13:         (o.p: number); // error: void ~> number
                               ^^^^^^ [2]: number

          test.js:15
           15:         (o.p: string); // error: void ~> string
                        ^^^ Cannot cast property \`p\` to string because boolean [1] is incompatible with string [2].
            References:
             15:         (o.p: string); // error: void ~> string
                          ^^^ [1]: boolean
             15:         (o.p: string); // error: void ~> string
                               ^^^^^^ [2]: string
        `,
      ),
  ]),
]);
