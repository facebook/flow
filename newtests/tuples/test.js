/*
 * @flow
 * @lint-ignore-every LINEWRAP1
 */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('$ReadOnlyArray<T> is the supertype for all tuples', [
    addCode(`
      function tupleLength(tup: $ReadOnlyArray<mixed>): number {
        // $ReadOnlyArray can use Array.prototype properties that don't mutate
        // it
        return tup.length;
      }
      // Array literals with known types can flow to $ReadOnlyArray
      tupleLength([1,2,3]);
      tupleLength(["a", "b", "c"]);
      // Arrays can flow to $ReadOnlyArray
      tupleLength(([1, 2, 3]: Array<number>));
      // Tuple types can flow to $ReadOnlyArray
      tupleLength(([1,2,3]: [1,2,3]));
      // $ReadOnlyArray can flow to $ReadOnlyArray
      tupleLength(([1,2,3]: $ReadOnlyArray<number>));
    `).noNewErrors(),

    addCode(`
      const elemCheck =
        (tup: $ReadOnlyArray<number>): $ReadOnlyArray<string> => tup;
    `).newErrors(
        `
          test.js:22
           22:         (tup: $ReadOnlyArray<number>): $ReadOnlyArray<string> => tup;
                                                                                ^^^ read-only array type. This type is incompatible with the expected return type of
           22:         (tup: $ReadOnlyArray<number>): $ReadOnlyArray<string> => tup;
                                                      ^^^^^^^^^^^^^^^^^^^^^^ read-only array type
            Type argument \`T\` is incompatible:
               22:         (tup: $ReadOnlyArray<number>): $ReadOnlyArray<string> => tup;
                                                ^^^^^^ number. This type is incompatible with
               22:         (tup: $ReadOnlyArray<number>): $ReadOnlyArray<string> => tup;
                                                                         ^^^^^^ string
        `,
      ),

    addCode(`
      const tupleToArray = (tup: $ReadOnlyArray<number>): Array<number> => tup;
    `).newErrors(
        `
          test.js:26
           26:       const tupleToArray = (tup: $ReadOnlyArray<number>): Array<number> => tup;
                                                                                          ^^^ read-only array type. This type is incompatible with the expected return type of
           26:       const tupleToArray = (tup: $ReadOnlyArray<number>): Array<number> => tup;
                                                                         ^^^^^^^^^^^^^ array type
        `,
      ),

    addCode(`
      const arrayMethods = (tup: $ReadOnlyArray<number>): void => tup.push(123);
    `).newErrors(
        `
          test.js:30
           30:       const arrayMethods = (tup: $ReadOnlyArray<number>): void => tup.push(123);
                                                                                     ^^^^ property \`push\`. Property not found in
           30:       const arrayMethods = (tup: $ReadOnlyArray<number>): void => tup.push(123);
                                                                                 ^^^ $ReadOnlyArray
        `,
      ),
  ]),
  test('You can use the $ReadOnlyArray functions', [
    addCode(
      `function foo(x: [1,2]): string { return x.length; }`
    ).newErrors(
       `
         test.js:3
           3: function foo(x: [1,2]): string { return x.length; }
                                                      ^^^^^^^^ number. This type is incompatible with the expected return type of
           3: function foo(x: [1,2]): string { return x.length; }
                                      ^^^^^^ string
       `,
     ),
  ]),
  test('The ref that $ReadOnlyArray functions provide is a $ReadOnlyArray', [
    addCode(`
      function foo(tup: [1,2], arr: Array<number>): void {
        tup.forEach((value, index, readOnlyRef) => {
          readOnlyRef.push(123);
          (readOnlyRef[0]: 1);
        });

        arr.forEach((value, index, writeableRef) => {
          writeableRef.push(123);
        });
      }
    `).newErrors(
        `
          test.js:6
            6:           readOnlyRef.push(123);
                                     ^^^^ property \`push\`. Property not found in
            6:           readOnlyRef.push(123);
                         ^^^^^^^^^^^ $ReadOnlyArray

          test.js:7
            7:           (readOnlyRef[0]: 1);
                          ^^^^^^^^^^^^^^ number literal \`2\`. Expected number literal \`1\`, got \`2\` instead
            7:           (readOnlyRef[0]: 1);
                                          ^ number literal \`1\`
        `,
      ),
  ]),
  test('You can\'t use Array functions', [
    addCode(
      `function foo(x: [1,2]): number { return x.unshift(); }`
    ).newErrors(
       `
         test.js:3
           3: function foo(x: [1,2]): number { return x.unshift(); }
                                                        ^^^^^^^ property \`unshift\`. Property not found in
           3: function foo(x: [1,2]): number { return x.unshift(); }
                                                      ^ $ReadOnlyArray
       `,
     ),
  ]),
  test('Arity is enforced bidirectionally', [
    addCode(`
      function foo(x: [1, 2]): [1] { return x; }
      function bar(x: [1]): [1, 2] { return x; }
    `).newErrors(
        `
          test.js:4
            4:       function foo(x: [1, 2]): [1] { return x; }
                                                           ^ tuple type. Tuple arity mismatch. This tuple has 2 elements and cannot flow to the 1 elements of
            4:       function foo(x: [1, 2]): [1] { return x; }
                                              ^^^ tuple type

          test.js:5
            5:       function bar(x: [1]): [1, 2] { return x; }
                                                           ^ tuple type. Tuple arity mismatch. This tuple has 1 elements and cannot flow to the 2 elements of
            5:       function bar(x: [1]): [1, 2] { return x; }
                                           ^^^^^^ tuple type
        `,
      )
  ]),
  test('The empty array literal is a 0-tuple', [
    addCode(`
      const foo: [] = [];
      (foo: [1]);
    `).newErrors(
        `
          test.js:5
            5:       (foo: [1]);
                      ^^^ tuple type. Tuple arity mismatch. This tuple has 0 elements and cannot flow to the 1 elements of
            5:       (foo: [1]);
                           ^^^ tuple type
        `,
      ),
  ]),
  test('It is an error to access a tuple out of bounds', [
    addCode('function foo(x: [1,2]): number { return x[2]; }')
      .newErrors(
        `
          test.js:3
            3: function foo(x: [1,2]): number { return x[2]; }
                                                       ^^^^ computed property. Out of bound access. This tuple has 2 elements and you tried to access index 2 of
            3: function foo(x: [1,2]): number { return x[2]; }
                                                       ^ tuple type

          test.js:3
            3: function foo(x: [1,2]): number { return x[2]; }
                                                       ^^^^ undefined (out of bounds tuple access). This type is incompatible with the expected return type of
            3: function foo(x: [1,2]): number { return x[2]; }
                                       ^^^^^^ number
        `,
      )
      .because('Out of bounds access causes an error and results in void'),
    addCode('function foo(x: [1,2]): number { return x[-1]; }')
      .newErrors(
        `
          test.js:5
            5: function foo(x: [1,2]): number { return x[-1]; }
                                                       ^^^^^ computed property. Out of bound access. This tuple has 2 elements and you tried to access index -1 of
            5: function foo(x: [1,2]): number { return x[-1]; }
                                                       ^ tuple type

          test.js:5
            5: function foo(x: [1,2]): number { return x[-1]; }
                                                       ^^^^^ undefined (out of bounds tuple access). This type is incompatible with the expected return type of
            5: function foo(x: [1,2]): number { return x[-1]; }
                                       ^^^^^^ number
        `,
      ),
  ]),
  test('Out of bounds access returns void', [
    addCode('function foo(x: [1]): string { return x[2]; }')
      .newErrors(
        `
          test.js:3
            3: function foo(x: [1]): string { return x[2]; }
                                                     ^^^^ computed property. Out of bound access. This tuple has 1 elements and you tried to access index 2 of
            3: function foo(x: [1]): string { return x[2]; }
                                                     ^ tuple type

          test.js:3
            3: function foo(x: [1]): string { return x[2]; }
                                                     ^^^^ undefined (out of bounds tuple access). This type is incompatible with the expected return type of
            3: function foo(x: [1]): string { return x[2]; }
                                     ^^^^^^ string
        `,
      ),
  ]),
  test('Unknown key access returns the general type', [
    addCode('function foo(x: [1], y: number): string { return x[y]; }')
      .newErrors(
        `
          test.js:3
            3: function foo(x: [1], y: number): string { return x[y]; }
                                                                ^^^^ number literal \`1\`. This type is incompatible with the expected return type of
            3: function foo(x: [1], y: number): string { return x[y]; }
                                                ^^^^^^ string
        `,
      ),
  ]),
  test('Array literals with known elements can flow to tuples', [
    addCode(`
      const arr = [1,2,3];
      (arr: [1,2,3]);
    `).noNewErrors(),
    addCode(`
      (arr: [1,2]);
      (arr: [1,2,3,4]);
    `).newErrors(
        `
          test.js:9
            9:       (arr: [1,2]);
                      ^^^ array literal. Tuple arity mismatch. This tuple has 3 elements and cannot flow to the 2 elements of
            9:       (arr: [1,2]);
                           ^^^^^ tuple type

          test.js:10
           10:       (arr: [1,2,3,4]);
                      ^^^ array literal. Tuple arity mismatch. This tuple has 3 elements and cannot flow to the 4 elements of
           10:       (arr: [1,2,3,4]);
                           ^^^^^^^^^ tuple type
        `,
      ).because('Arity is enforced'),
  ]),
  test('Array literals without known elements cannot flow to tuples', [
    addCode(`
      function foo(arr: Array<number>): [number, number] { return arr; }
    `).newErrors(
        `
          test.js:4
            4:       function foo(arr: Array<number>): [number, number] { return arr; }
                                                                                 ^^^ array type. Only tuples and array literals with known elements can flow to
            4:       function foo(arr: Array<number>): [number, number] { return arr; }
                                                       ^^^^^^^^^^^^^^^^ tuple type
        `,
      ),
  ]),
  test('Tuples cannot flow to arrays at the moment', [
    addCode(`
      function foo(arr: [1,2]): Array<number> { return arr; }
    `).newErrors(
        `
          test.js:4
            4:       function foo(arr: [1,2]): Array<number> { return arr; }
                                                                      ^^^ tuple type. This type is incompatible with the expected return type of
            4:       function foo(arr: [1,2]): Array<number> { return arr; }
                                               ^^^^^^^^^^^^^ array type
        `,
      ).because('Tuples are subtypes of arrays'),
  ]),
  test('Destructuring tuple should eat the first few elements', [
    addCode(`
      const tup: [1,2,3,4] = [1,2,3,4];
      const [a, b, ...rest] = tup;
      (a: 10);
      (b: 20);
      (rest: [3,40]);
    `).newErrors(
        `
          test.js:6
            6:       (a: 10);
                      ^ number literal \`1\`. Expected number literal \`10\`, got \`1\` instead
            6:       (a: 10);
                         ^^ number literal \`10\`

          test.js:7
            7:       (b: 20);
                      ^ number literal \`2\`. Expected number literal \`20\`, got \`2\` instead
            7:       (b: 20);
                         ^^ number literal \`20\`

          test.js:8
            8:       (rest: [3,40]);
                      ^^^^ rest. Has some incompatible tuple element with
            8:       (rest: [3,40]);
                            ^^^^^^ tuple type
            The second tuple element is incompatible:
                4:       const tup: [1,2,3,4] = [1,2,3,4];
                                           ^ number literal \`4\`. Expected number literal \`40\`, got \`4\` instead
                8:       (rest: [3,40]);
                                   ^^ number literal \`40\`

          test.js:8
            8:       (rest: [3,40]);
                      ^^^^ rest. Has some incompatible tuple element with
            8:       (rest: [3,40]);
                            ^^^^^^ tuple type
            The second tuple element is incompatible:
                8:       (rest: [3,40]);
                                   ^^ number literal \`40\`. Expected number literal \`4\`, got \`40\` instead
                4:       const tup: [1,2,3,4] = [1,2,3,4];
                                           ^ number literal \`4\`
        `,
      ),
  ]),
  test('$TupleMap should still work as a lower bound', [
    addCode(`
      function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
        return arr;
      }
    `).newErrors(
        `
          test.js:5
            5:         return arr;
                              ^^^ array type. This type is incompatible with the expected return type of
            4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                                                                       ^^^^^^ tuple type
            The first tuple element is incompatible:
                4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string. This type is incompatible with
                4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                                                                            ^ number literal \`1\`

          test.js:5
            5:         return arr;
                              ^^^ array type. This type is incompatible with the expected return type of
            4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                                                                       ^^^^^^ tuple type
            The second tuple element is incompatible:
                4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string. This type is incompatible with
                4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                                                                               ^ number literal \`2\`
        `,
      ),
  ]),
  test('$TupleMap should still work as a upper bound', [
    addCode(`
      function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
        return arr;
      }
    `).newErrors(
        `
          test.js:5
            5:         return arr;
                              ^^^ tuple type. This type is incompatible with the expected return type of
            4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ array type
            The first tuple element is incompatible:
                4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                            ^^^^^^ number. This type is incompatible with
                4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string

          test.js:5
            5:         return arr;
                              ^^^ tuple type. This type is incompatible with the expected return type of
            4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ array type
            The second tuple element is incompatible:
                4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                                    ^^^^^^ number. This type is incompatible with
                4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string
        `,
      ),
   ]),
  test('Tuple elements cannot be subtyped', [
    addCode(`
      function foo(tup: [1, 2]): [number, number] {
        return tup;
      }
    `).newErrors(
        `
          test.js:5
            5:         return tup;
                              ^^^ tuple type. This type is incompatible with the expected return type of
            4:       function foo(tup: [1, 2]): [number, number] {
                                                ^^^^^^^^^^^^^^^^ tuple type
            The first tuple element is incompatible:
                4:       function foo(tup: [1, 2]): [number, number] {
                                                     ^^^^^^ number. Expected number literal \`1\`
                4:       function foo(tup: [1, 2]): [number, number] {
                                            ^ number literal \`1\`

          test.js:5
            5:         return tup;
                              ^^^ tuple type. This type is incompatible with the expected return type of
            4:       function foo(tup: [1, 2]): [number, number] {
                                                ^^^^^^^^^^^^^^^^ tuple type
            The second tuple element is incompatible:
                4:       function foo(tup: [1, 2]): [number, number] {
                                                             ^^^^^^ number. Expected number literal \`2\`
                4:       function foo(tup: [1, 2]): [number, number] {
                                               ^ number literal \`2\`
        `,
      ),
  ]),
  test('Fresh array -> tuple can subtype', [
    addCode(`
      var arr: [number | string] = [123];
    `).noNewErrors(),
  ]),
  test('instanceof Array works for tuples', [
    addCode(`
      function foo(tup: ?[number, number]): number {
        if (tup instanceof Array) {
          return tup[3];
        } else {
          return 123;
        }
      }
    `)
      .newErrors(
        `
          test.js:6
            6:           return tup[3];
                                ^^^^^^ computed property. Out of bound access. This tuple has 2 elements and you tried to access index 3 of
            6:           return tup[3];
                                ^^^ tuple type

          test.js:6
            6:           return tup[3];
                                ^^^^^^ undefined (out of bounds tuple access). This type is incompatible with the expected return type of
            4:       function foo(tup: ?[number, number]): number {
                                                           ^^^^^^ number
        `,
      ),
  ]),
]);
