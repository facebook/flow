/* @flow */

import {suite, test} from '../../tsrc/test/Tester';

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
               ^^^ number. This type is incompatible with
           8: (o.p: string);
                    ^^^^^^ string
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
                      ^^^ boolean. This type is incompatible with
            7:       (a.p: string);
                           ^^^^^^ string

          test.js:8
            8:       (b.p: number);
                      ^^^ boolean. This type is incompatible with
            8:       (b.p: number);
                           ^^^^^^ number
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
                ^^^ number. This type is incompatible with
           12: (a.p: void);
                     ^^^^ undefined
        `,
      ),

    // *not* number ~> void
    addCode(`(b.p: void);`)
      .newErrors(
        `
          test.js:14
           14: (b.p: void);
                ^^^ string. This type is incompatible with
           14: (b.p: void);
                     ^^^^ undefined
        `,
      ),

    // TODO: This should add more errors
    addCode(`proto.p = true;`)
      .newErrors(
        `
          test.js:12
           12: (a.p: void);
                ^^^ boolean. This type is incompatible with
           12: (a.p: void);
                     ^^^^ undefined

          test.js:12
           12: (a.p: void);
                ^^^ string. This type is incompatible with
           12: (a.p: void);
                     ^^^^ undefined

          test.js:14
           14: (b.p: void);
                ^^^ boolean. This type is incompatible with
           14: (b.p: void);
                     ^^^^ undefined

          test.js:14
           14: (b.p: void);
                ^^^ number. This type is incompatible with
           14: (b.p: void);
                     ^^^^ undefined
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
          test.js:5
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
                         ^ number. This type is incompatible with
            7: var o: {p: string} = Object.create(proto);
                          ^^^^^^ string
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
                        ^^^ boolean. This type is incompatible with
            7:         (o.p: number);
                             ^^^^^^ number

          test.js:9
            9:         (o.p: string);
                        ^^^ boolean. This type is incompatible with
            9:         (o.p: string);
                             ^^^^^^ string
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
                        ^^^ number. This type is incompatible with
            7:         (o.p: void);
                             ^^^^ undefined

          test.js:7
            7:         (o.p: void);
                        ^^^ string. This type is incompatible with
            7:         (o.p: void);
                             ^^^^ undefined
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
                        ^^^ boolean. This type is incompatible with
            9:         (a.p: number);
                             ^^^^^^ number

          test.js:11
           11:         (b.p: string);
                        ^^^ boolean. This type is incompatible with
           11:         (b.p: string);
                             ^^^^^^ string
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
                ^^^ number. This type is incompatible with
           13: (o.p: void);
                     ^^^^ undefined

          test.js:13
           13: (o.p: void);
                ^^^ string. This type is incompatible with
           13: (o.p: void);
                     ^^^^ undefined
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
                        ^^^ boolean. This type is incompatible with
           13:         (o.p: number); // error: void ~> number
                             ^^^^^^ number

          test.js:15
           15:         (o.p: string); // error: void ~> string
                        ^^^ boolean. This type is incompatible with
           15:         (o.p: string); // error: void ~> string
                             ^^^^^^ string
        `,
      ),
  ]),
]);
