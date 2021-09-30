/*
 * @flow
 */

import type {Suite} from "flow-dev-tools/src/test/Suite";
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(({addFile, addFiles, addCode}) => [
  test('idx(object)', [
    addCode('declare var idx: $Facebookism$Idx;\n').noNewErrors(),
    addCode('declare var obj1: {a: ?{b: {c: number}}};').noNewErrors(),
    addCode('obj1.a.b.c;\n')
      .newErrors(
        `
          test.js:8
            8: obj1.a.b.c;
                      ^ Cannot get \`obj1.a.b\` because property \`b\` is missing in null or undefined [1]. [incompatible-use]
            References:
              6: declare var obj1: {a: ?{b: {c: number}}};
                                       ^^^^^^^^^^^^^^^^^ [1]
        `,
      ),
    addCode('(idx(obj1, obj => obj.a.b.c): ?number);\n').noNewErrors(),
    addCode('(idx(obj1, obj => obj["a"].b.c): ?number);\n').noNewErrors(),
    addCode('(idx(obj1, obj => obj.a.b.c): number);\n').
      newErrors(
       `
         test.js:17
          17: (idx(obj1, obj => obj.a.b.c): number);
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to number because null or undefined [1] is incompatible with number [2]. [incompatible-cast]
           References:
            17: (idx(obj1, obj => obj.a.b.c): number);
                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
            17: (idx(obj1, obj => obj.a.b.c): number);
                                              ^^^^^^ [2]
       `,
     ),
    addCode('(idx(obj1, obj => obj.a.b.c): ?string);\n')
      .newErrors(
        `
          test.js:20
           20: (idx(obj1, obj => obj.a.b.c): ?string);
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to nullable string because number [1] is incompatible with string [2]. [incompatible-cast]
            References:
              6: declare var obj1: {a: ?{b: {c: number}}};
                                                ^^^^^^ [1]
             20: (idx(obj1, obj => obj.a.b.c): ?string);
                                                ^^^^^^ [2]
        `,
      ),
    addCode('(idx(obj1, obj => obj["a"].b.c): number);\n')
      .newErrors(
        `
          test.js:23
           23: (idx(obj1, obj => obj["a"].b.c): number);
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to number because null or undefined [1] is incompatible with number [2]. [incompatible-cast]
            References:
             23: (idx(obj1, obj => obj["a"].b.c): number);
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
             23: (idx(obj1, obj => obj["a"].b.c): number);
                                                  ^^^^^^ [2]
        `,
      ),
    addCode('idx(obj1, obj => obj.notAProp);\n')
      .newErrors(
        `
          test.js:26
           26: idx(obj1, obj => obj.notAProp);
                                    ^^^^^^^^ Cannot get \`obj.notAProp\` because property \`notAProp\` is missing in object type [1]. [prop-missing]
            References:
              6: declare var obj1: {a: ?{b: {c: number}}};
                                   ^^^^^^^^^^^^^^^^^^^^^^ [1]
        `,
      ),
    addCode('idx(obj1, obj => obj.a = null);\n')
      .newErrors(
        `
          test.js:29
           29: idx(obj1, obj => obj.a = null);
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot call \`idx(...)\` because the callback must only access properties on the callback parameter. [invalid-idx]
        `,
      ),
    addCode('declare var obj2: {a?: {b: {c: number}}};').noNewErrors(),
    addCode('(idx(obj2, obj => obj.a.b.c): ?number);\n').noNewErrors(),
    addCode('(idx(obj2, obj => obj.a.b.c): number);\n')
      .newErrors(
        `
          test.js:37
           37: (idx(obj2, obj => obj.a.b.c): number);
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to number because null or undefined [1] is incompatible with number [2]. [incompatible-cast]
            References:
             37: (idx(obj2, obj => obj.a.b.c): number);
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
             37: (idx(obj2, obj => obj.a.b.c): number);
                                               ^^^^^^ [2]
        `,
      ),
    addCode('declare var obj3: {a: null | {b: {c: number}}};').noNewErrors(),
    addCode('(idx(obj3, obj => obj.a.b.c): ?number);\n').noNewErrors(),
    addCode('(idx(obj3, obj => obj.a.b.c): number);\n')
      .newErrors(
        `
          test.js:45
           45: (idx(obj3, obj => obj.a.b.c): number);
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to number because null or undefined [1] is incompatible with number [2]. [incompatible-cast]
            References:
             45: (idx(obj3, obj => obj.a.b.c): number);
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
             45: (idx(obj3, obj => obj.a.b.c): number);
                                               ^^^^^^ [2]
        `,
      ),
    // Nested maybes/optionals should get unwrapped
    addCode('declare var obj4: {a?: ?(?{b: number})};').noNewErrors(),
    addCode('(idx(obj4, obj => obj.a.b): ?number)').noNewErrors(),
  ]),

  test('unions', [
    addCode('declare var idx: $Facebookism$Idx;\n').noNewErrors(),
    addCode('declare var ab: {a:string}|{b:number};\n').noNewErrors(),
    addCode('(idx(ab, _ => _.a): empty);\n')
      .newErrors(
        `
          test.js:9
            9: (idx(ab, _ => _.a): empty);
                ^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to empty because string [1] is incompatible with empty [2]. [incompatible-cast]
            References:
              6: declare var ab: {a:string}|{b:number};
                                    ^^^^^^ [1]
              9: (idx(ab, _ => _.a): empty);
                                     ^^^^^ [2]

          test.js:9
            9: (idx(ab, _ => _.a): empty);
                ^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to empty because null or undefined [1] is incompatible with empty [2]. [incompatible-cast]
            References:
              9: (idx(ab, _ => _.a): empty);
                  ^^^^^^^^^^^^^^^^^ [1]
              9: (idx(ab, _ => _.a): empty);
                                     ^^^^^ [2]

          test.js:9
            9: (idx(ab, _ => _.a): empty);
                               ^ Cannot get \`_.a\` because property \`a\` is missing in object type [1]. [prop-missing]
            References:
              6: declare var ab: {a:string}|{b:number};
                                            ^^^^^^^^^^ [1]
        `,
      ),
    addCode('(idx(ab, _ => _.b): empty);\n')
      .newErrors(
        `
          test.js:12
           12: (idx(ab, _ => _.b): empty);
                ^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to empty because number [1] is incompatible with empty [2]. [incompatible-cast]
            References:
              6: declare var ab: {a:string}|{b:number};
                                               ^^^^^^ [1]
             12: (idx(ab, _ => _.b): empty);
                                     ^^^^^ [2]

          test.js:12
           12: (idx(ab, _ => _.b): empty);
                ^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to empty because null or undefined [1] is incompatible with empty [2]. [incompatible-cast]
            References:
             12: (idx(ab, _ => _.b): empty);
                  ^^^^^^^^^^^^^^^^^ [1]
             12: (idx(ab, _ => _.b): empty);
                                     ^^^^^ [2]

          test.js:12
           12: (idx(ab, _ => _.b): empty);
                               ^ Cannot get \`_.b\` because property \`b\` is missing in object type [1]. [prop-missing]
            References:
              6: declare var ab: {a:string}|{b:number};
                                 ^^^^^^^^^^ [1]
        `,
      ),
    addCode('(idx(ab, _ => _.c): empty);\n')
      .newErrors(
        `
          test.js:15
           15: (idx(ab, _ => _.c): empty);
                ^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to empty because null or undefined [1] is incompatible with empty [2]. [incompatible-cast]
            References:
             15: (idx(ab, _ => _.c): empty);
                  ^^^^^^^^^^^^^^^^^ [1]
             15: (idx(ab, _ => _.c): empty);
                                     ^^^^^ [2]

          test.js:15
           15: (idx(ab, _ => _.c): empty);
                               ^ Cannot get \`_.c\` because property \`c\` is missing in object type [1]. [prop-missing]
            References:
              6: declare var ab: {a:string}|{b:number};
                                 ^^^^^^^^^^ [1]

          test.js:15
           15: (idx(ab, _ => _.c): empty);
                               ^ Cannot get \`_.c\` because property \`c\` is missing in object type [1]. [prop-missing]
            References:
              6: declare var ab: {a:string}|{b:number};
                                            ^^^^^^^^^^ [1]
        `,
      ),
  ]),

  test('idx(classInst)', [
    addCode('declare var idx: $Facebookism$Idx;\n').noNewErrors(),
    addCode('class Foo1 { a: ?Foo1; b: ?number; }\n').noNewErrors(),
    addCode('class Foo2 { a: Foo2 | void; b: ?number; }\n').noNewErrors(),
    addCode('class Foo3 { a: Foo3 | null; b: ?number; }\n').noNewErrors(),

    addCode('(idx(new Foo1(), o => o.a.b): ?number);\n').noNewErrors(),
    addCode('(idx(new Foo1(), o => o.a.b): number);\n')
      .newErrors(
        `
          test.js:18
           18: (idx(new Foo1(), o => o.a.b): number);
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to number because null or undefined [1] is incompatible with number [2]. [incompatible-cast]
            References:
              6: class Foo1 { a: ?Foo1; b: ?number; }
                                           ^^^^^^^ [1]
             18: (idx(new Foo1(), o => o.a.b): number);
                                               ^^^^^^ [2]

          test.js:18
           18: (idx(new Foo1(), o => o.a.b): number);
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to number because null or undefined [1] is incompatible with number [2]. [incompatible-cast]
            References:
             18: (idx(new Foo1(), o => o.a.b): number);
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^ [1]
             18: (idx(new Foo1(), o => o.a.b): number);
                                               ^^^^^^ [2]
        `,
      ),
    addCode('idx(new Foo1(), o => o.a = null);\n')
      .newErrors(
        `
          test.js:21
           21: idx(new Foo1(), o => o.a = null);
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ Cannot call \`idx(...)\` because the callback must only access properties on the callback parameter. [invalid-idx]
        `,
      ),
  ]),

  test('idx(array)', [
    addCode('declare var idx: $Facebookism$Idx;\n').noNewErrors(),
    addCode('declare var arr1: Array<?Array<number>>;\n').noNewErrors(),
    addCode('declare var arr2: Array<Array<number> | void>;\n').noNewErrors(),
    addCode('declare var arr3: Array<Array<number> | null>;\n').noNewErrors(),

    addCode('(idx(arr1, arr => arr[0][0]): ?number);\n').noNewErrors(),
    addCode('(idx(arr2, arr => arr[0][0]): ?number);\n').noNewErrors(),
    addCode('(idx(arr3, arr => arr[0][0]): ?number);\n').noNewErrors(),
  ]),

  test('idx(nonObject)', [
    addCode('declare var idx: $Facebookism$Idx;\n').noNewErrors(),

    addCode('(idx(42, n => n): ?number);\n').noNewErrors(),
    addCode('(idx(42, n => n): number);\n')
      .newErrors(
        `
          test.js:9
            9: (idx(42, n => n): number);
                ^^^^^^^^^^^^^^^ Cannot cast \`idx(...)\` to number because null or undefined [1] is incompatible with number [2]. [incompatible-cast]
            References:
              9: (idx(42, n => n): number);
                  ^^^^^^^^^^^^^^^ [1]
              9: (idx(42, n => n): number);
                                   ^^^^^^ [2]
        `,
      ),
    addCode('idx(42, n => n.nope);\n')
      .newErrors(
        `
          test.js:12
           12: idx(42, n => n.nope);
                              ^^^^ Cannot get \`n.nope\` because property \`nope\` is missing in \`Number\` [1]. [prop-missing]
            References:
             12: idx(42, n => n.nope);
                     ^^ [1]
        `,
      ),
  ]),

  test('idx() weird edge cases', [
    addCode('declare var idx: $Facebookism$Idx;\n').noNewErrors(),

    // Using an annotation obscures the type wrapper mechanism that idx() uses
    // around the parameter it passes to the callback
    addCode('(idx({}, (obj: Object) => obj.a.b.c): ?number);\n')
      .noNewErrors(),

    // Can't do anything with the callback parameter other than get elements and
    // properties off of it
    addCode('idx({}, obj => obj());\n')
      .newErrors(
        `
          test.js:9
            9: idx({}, obj => obj());
               ^^^^^^^^^^^^^^^^^^^^^ Cannot call \`idx(...)\` because the callback must only access properties on the callback parameter. [invalid-idx]
        `,
      ),
  ]),
]): Suite);
