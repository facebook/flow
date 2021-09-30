/*
 * @flow
 */

import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFile, addFiles, addCode}) => [
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
               ^^^ Cannot cast \`o.p\` to string because number [1] is incompatible with string [2]. [incompatible-cast]
           References:
            10: proto.p = 0;
                          ^ [1]
             8: (o.p: string);
                      ^^^^^^ [2]
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
                      ^^^ Cannot cast \`a.p\` to string because boolean [1] is incompatible with string [2]. [incompatible-cast]
            References:
             11: proto.p = true;
                           ^^^^ [1]
              7:       (a.p: string);
                             ^^^^^^ [2]

          test.js:8
            8:       (b.p: number);
                      ^^^ Cannot cast \`b.p\` to number because boolean [1] is incompatible with number [2]. [incompatible-cast]
            References:
             11: proto.p = true;
                           ^^^^ [1]
              8:       (b.p: number);
                             ^^^^^^ [2]
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
                ^^^ Cannot cast \`a.p\` to undefined because number [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
              7:       a.p = 0;
                             ^ [1]
             12: (a.p: void);
                       ^^^^ [2]
        `,
      ),

    // *not* number ~> void
    addCode(`(b.p: void);`)
      .newErrors(
        `
          test.js:14
           14: (b.p: void);
                ^^^ Cannot cast \`b.p\` to undefined because string [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
              8:       b.p = "";
                             ^^ [1]
             14: (b.p: void);
                       ^^^^ [2]
        `,
      ),

    // TODO: This should add more errors
    addCode(`proto.p = true;`)
      .newErrors(
        `
          test.js:12
           12: (a.p: void);
                ^^^ Cannot cast \`a.p\` to undefined because string [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
              8:       b.p = "";
                             ^^ [1]
             12: (a.p: void);
                       ^^^^ [2]

          test.js:12
           12: (a.p: void);
                ^^^ Cannot cast \`a.p\` to undefined because boolean [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
             16: proto.p = true;
                           ^^^^ [1]
             12: (a.p: void);
                       ^^^^ [2]

          test.js:14
           14: (b.p: void);
                ^^^ Cannot cast \`b.p\` to undefined because number [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
              7:       a.p = 0;
                             ^ [1]
             14: (b.p: void);
                       ^^^^ [2]

          test.js:14
           14: (b.p: void);
                ^^^ Cannot cast \`b.p\` to undefined because boolean [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
             16: proto.p = true;
                           ^^^^ [1]
             14: (b.p: void);
                       ^^^^ [2]
        `,
      ),
  ]),

  test('derived object subtyping -- write before read', [
    addCode(`
      var proto = {};
      proto.p = 0;
    `).noNewErrors(),

    addCode(`var o: {p: string} = Object.create(proto);`)
      .noNewErrors(),
  ]),

  test('derived object subtyping -- read before write', [
    addCode(`
      var proto = {};
    `).noNewErrors(),

    /* NOTE: Shadow reads for ObjT -> ObjT are currently strict, which is
       inconsistent with GetPropT/MethodT. It would be confusing if this
       didn't error, though: var o: { p: string } = {} */
    addCode(`var o: {p: string} = Object.create(proto);`)
      .noNewErrors(),

    addCode(`proto.p = 0;`)
      .noNewErrors(),
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
                        ^^^ Cannot cast \`o.p\` to number because boolean [1] is incompatible with number [2]. [incompatible-cast]
            References:
             13: o.p = true;
                       ^^^^ [1]
              7:         (o.p: number);
                               ^^^^^^ [2]

          test.js:9
            9:         (o.p: string);
                        ^^^ Cannot cast \`o.p\` to string because boolean [1] is incompatible with string [2]. [incompatible-cast]
            References:
             13: o.p = true;
                       ^^^^ [1]
              9:         (o.p: string);
                               ^^^^^^ [2]
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
                        ^^^ Cannot cast \`o.p\` to undefined because number [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
             13:         o.p = 0;
                               ^ [1]
              7:         (o.p: void);
                               ^^^^ [2]

          test.js:7
            7:         (o.p: void);
                        ^^^ Cannot cast \`o.p\` to undefined because string [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
             15:         o.p = "";
                               ^^ [1]
              7:         (o.p: void);
                               ^^^^ [2]
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
                        ^^^ Cannot cast \`a.p\` to number because boolean [1] is incompatible with number [2]. [incompatible-cast]
            References:
             15: proto.p = true;
                           ^^^^ [1]
              9:         (a.p: number);
                               ^^^^^^ [2]

          test.js:11
           11:         (b.p: string);
                        ^^^ Cannot cast \`b.p\` to string because boolean [1] is incompatible with string [2]. [incompatible-cast]
            References:
             15: proto.p = true;
                           ^^^^ [1]
             11:         (b.p: string);
                               ^^^^^^ [2]
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
                ^^^ Cannot cast \`o.p\` to undefined because number [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
              7:         o.p = 0;
                               ^ [1]
             13: (o.p: void);
                       ^^^^ [2]

          test.js:13
           13: (o.p: void);
                ^^^ Cannot cast \`o.p\` to undefined because string [1] is incompatible with undefined [2]. [incompatible-cast]
            References:
              9:         o.p = "";
                               ^^ [1]
             13: (o.p: void);
                       ^^^^ [2]
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
                        ^^^ Cannot cast \`o.p\` to number because boolean [1] is incompatible with number [2]. [incompatible-cast]
            References:
              7:         o.p = true;
                               ^^^^ [1]
             13:         (o.p: number); // error: void ~> number
                               ^^^^^^ [2]

          test.js:15
           15:         (o.p: string); // error: void ~> string
                        ^^^ Cannot cast \`o.p\` to string because boolean [1] is incompatible with string [2]. [incompatible-cast]
            References:
              7:         o.p = true;
                               ^^^^ [1]
             15:         (o.p: string); // error: void ~> string
                               ^^^^^^ [2]
        `,
      ),
  ]),
]): Suite);
