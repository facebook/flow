/* @flow */

import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('idx(object)', [
    addCode('declare var idx: $Facebookism$Idx;\n').noNewErrors(),

    addCode('declare var obj1: {a: ?{b: {c: number}}};').noNewErrors(),
    addCode('obj1.a.b.c;\n').newErrors(
       `
         test.js:8
           8: obj1.a.b.c;
                     ^ property \`b\`. Property cannot be accessed on possibly null value
           8: obj1.a.b.c;
              ^^^^^^ null

         test.js:8
           8: obj1.a.b.c;
                     ^ property \`b\`. Property cannot be accessed on possibly undefined value
           8: obj1.a.b.c;
              ^^^^^^ undefined
       `,
    ),
    addCode('(idx(obj1, obj => obj.a.b.c): ?number);\n').noNewErrors(),
    addCode('(idx(obj1, obj => obj["a"].b.c): ?number);\n').noNewErrors(),
    addCode('(idx(obj1, obj => obj.a.b.c): number);\n').newErrors(
      `
        test.js:17
         17: (idx(obj1, obj => obj.a.b.c): number);
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^ null. This type is incompatible with
         17: (idx(obj1, obj => obj.a.b.c): number);
                                           ^^^^^^ number

        test.js:17
         17: (idx(obj1, obj => obj.a.b.c): number);
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^ undefined. This type is incompatible with
         17: (idx(obj1, obj => obj.a.b.c): number);
                                           ^^^^^^ number
      `,
    ),
    addCode('(idx(obj1, obj => obj.a.b.c): ?string);\n').newErrors(
      `
        test.js:20
         20: (idx(obj1, obj => obj.a.b.c): ?string);
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^ number. This type is incompatible with
         20: (idx(obj1, obj => obj.a.b.c): ?string);
                                            ^^^^^^ string
      `,
    ),

    addCode('(idx(obj1, obj => obj["a"].b.c): number);\n').newErrors(
       `
         test.js:23
          23: (idx(obj1, obj => obj["a"].b.c): number);
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ null. This type is incompatible with
          23: (idx(obj1, obj => obj["a"].b.c): number);
                                               ^^^^^^ number

         test.js:23
          23: (idx(obj1, obj => obj["a"].b.c): number);
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ undefined. This type is incompatible with
          23: (idx(obj1, obj => obj["a"].b.c): number);
                                               ^^^^^^ number
       `,
    ),
    addCode('idx(obj1, obj => obj.notAProp);\n').newErrors(
       `
         test.js:26
          26: idx(obj1, obj => obj.notAProp);
                                   ^^^^^^^^ property \`notAProp\`. Property not found in
          26: idx(obj1, obj => obj.notAProp);
                               ^^^ object type
       `,
    ),
    addCode('idx(obj1, obj => obj.a = null);\n').newErrors(
       `
         test.js:29
          29: idx(obj1, obj => obj.a = null);
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call. idx() callbacks may only access properties on the callback parameter!
       `,
    ),

    addCode('declare var obj2: {a?: {b: {c: number}}};').noNewErrors(),
    addCode('(idx(obj2, obj => obj.a.b.c): ?number);\n').noNewErrors(),
    addCode('(idx(obj2, obj => obj.a.b.c): number);\n').newErrors(
      `
      test.js:37
      37: (idx(obj2, obj => obj.a.b.c): number);
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^ null. This type is incompatible with
      37: (idx(obj2, obj => obj.a.b.c): number);
                                        ^^^^^^ number

      test.js:37
      37: (idx(obj2, obj => obj.a.b.c): number);
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^ undefined. This type is incompatible with
      37: (idx(obj2, obj => obj.a.b.c): number);
                                        ^^^^^^ number
      `,
    ),

    addCode('declare var obj3: {a: null | {b: {c: number}}};').noNewErrors(),
    addCode('(idx(obj3, obj => obj.a.b.c): ?number);\n').noNewErrors(),
    addCode('(idx(obj3, obj => obj.a.b.c): number);\n').newErrors(
      `
      test.js:45
      45: (idx(obj3, obj => obj.a.b.c): number);
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^ null. This type is incompatible with
      45: (idx(obj3, obj => obj.a.b.c): number);
                                        ^^^^^^ number

      test.js:45
      45: (idx(obj3, obj => obj.a.b.c): number);
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^ undefined. This type is incompatible with
      45: (idx(obj3, obj => obj.a.b.c): number);
                                        ^^^^^^ number
      `,
    ),

    // Nested maybes/optionals should get unwrapped
    addCode('declare var obj4: {a?: ?(?{b: number})};').noNewErrors(),
    addCode('(idx(obj4, obj => obj.a.b): ?number)').noNewErrors(),
  ]),

  test('idx(classInst)', [
    addCode('declare var idx: $Facebookism$Idx;\n').noNewErrors(),
    addCode('class Foo1 { a: ?Foo1; b: ?number; }\n').noNewErrors(),
    addCode('class Foo2 { a: Foo2 | void; b: ?number; }\n').noNewErrors(),
    addCode('class Foo3 { a: Foo3 | null; b: ?number; }\n').noNewErrors(),

    addCode('(idx(new Foo1(), o => o.a.b): ?number);\n').noNewErrors(),
    addCode('(idx(new Foo1(), o => o.a.b): number);\n').newErrors(
      `
        test.js:18
         18: (idx(new Foo1(), o => o.a.b): number);
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^ null. This type is incompatible with
         18: (idx(new Foo1(), o => o.a.b): number);
                                          ^^^^^^ number

        test.js:18
         18: (idx(new Foo1(), o => o.a.b): number);
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^ undefined. This type is incompatible with
         18: (idx(new Foo1(), o => o.a.b): number);
                                          ^^^^^^ number
      `,
    ),
    addCode('idx(new Foo1(), o => o.a = null);\n').newErrors(
      `
        test.js:21
         21: idx(new Foo1(), o => o.a = null);
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call. idx() callbacks may only access properties on the callback parameter!
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
    addCode('(idx(42, n => n): number);\n').newErrors(
      `
        test.js:9
          9: (idx(42, n => n): number);
              ^^^^^^^^^^^^^^^ null. This type is incompatible with
          9: (idx(42, n => n): number);
                               ^^^^^^ number

        test.js:9
          9: (idx(42, n => n): number);
              ^^^^^^^^^^^^^^^ undefined. This type is incompatible with
          9: (idx(42, n => n): number);
                               ^^^^^^ number
      `,
    ),
    addCode('idx(42, n => n.nope);\n').newErrors(
      `
        test.js:12
         12: idx(42, n => n.nope);
                            ^^^^ property \`nope\`. Property not found in
         12: idx(42, n => n.nope);
                          ^ Number
      `,
    ),
  ]),

  test('idx() weird edge cases', [
    addCode('declare var idx: $Facebookism$Idx;\n').noNewErrors(),

    // Using an annotation obscures the type wrapper mechanism that idx() uses
    // around the parameter it passes to the callback
    addCode('(idx({}, (obj: Object) => obj.a.b.c): ?number);\n').newErrors(
       `
         test.js:6
           6: (idx({}, (obj: Object) => obj.a.b.c): ?number);
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call. idx() callback functions may not be annotated and they may only access properties on the callback parameter!
       `,
    ),

    // Can't do anything with the callback parameter other than get elements and
    // properties off of it
    addCode('idx({}, obj => obj());\n').newErrors(
      `
        test.js:9
          9: idx({}, obj => obj());
             ^^^^^^^^^^^^^^^^^^^^^ function call. idx() callbacks may only access properties on the callback parameter!
      `,
    ),
  ]),
]);
