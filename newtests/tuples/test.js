/*
 * @flow
 */


import {suite, test} from 'flow-dev-tools/src/test/Tester';

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
                                                                                ^^^ Cannot return \`tup\` because number [1] is incompatible with string [2] in array element.
            References:
             22:         (tup: $ReadOnlyArray<number>): $ReadOnlyArray<string> => tup;
                                              ^^^^^^ [1]
             22:         (tup: $ReadOnlyArray<number>): $ReadOnlyArray<string> => tup;
                                                                       ^^^^^^ [2]
        `,
      ),

    addCode(`
      const tupleToArray = (tup: $ReadOnlyArray<number>): Array<number> => tup;
    `).newErrors(
        `
          test.js:26
           26:       const tupleToArray = (tup: $ReadOnlyArray<number>): Array<number> => tup;
                                                                                          ^^^ Cannot return \`tup\` because read-only array type [1] is incompatible with array type [2].
            References:
             26:       const tupleToArray = (tup: $ReadOnlyArray<number>): Array<number> => tup;
                                                  ^^^^^^^^^^^^^^^^^^^^^^ [1]
             26:       const tupleToArray = (tup: $ReadOnlyArray<number>): Array<number> => tup;
                                                                           ^^^^^^^^^^^^^ [2]
        `,
      ),

    addCode(`
      const arrayMethods = (tup: $ReadOnlyArray<number>): void => tup.push(123);
    `).newErrors(
        `
          test.js:30
           30:       const arrayMethods = (tup: $ReadOnlyArray<number>): void => tup.push(123);
                                                                                     ^^^^ Cannot call \`tup.push\` because property \`push\` is missing in \`$ReadOnlyArray\` [1].
            References:
             30:       const arrayMethods = (tup: $ReadOnlyArray<number>): void => tup.push(123);
                                                  ^^^^^^^^^^^^^^^^^^^^^^ [1]
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
                                                      ^^^^^^^^ Cannot return \`x.length\` because length \`2\` (number) of tuple [1] is incompatible with string [2].
           References:
             3: function foo(x: [1,2]): string { return x.length; }
                                ^^^^^ [1]
             3: function foo(x: [1,2]): string { return x.length; }
                                        ^^^^^^ [2]
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
                                     ^^^^ Cannot call \`readOnlyRef.push\` because property \`push\` is missing in \`$ReadOnlyArray\` [1].
            References:
            228:     forEach(callbackfn: (value: T, index: number, array: $ReadOnlyArray<T>) => mixed, thisArg?: any): void;
                                                                          ^^^^^^^^^^^^^^^^^ [1]. See lib: [LIB] core.js:228

          test.js:7
            7:           (readOnlyRef[0]: 1);
                          ^^^^^^^^^^^^^^ Cannot cast \`readOnlyRef[0]\` to number literal \`1\` because number literal \`2\` [1] is incompatible with number literal \`1\` [2].
            References:
              4:       function foo(tup: [1,2], arr: Array<number>): void {
                                            ^ [1]
              7:           (readOnlyRef[0]: 1);
                                            ^ [2]
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
                                                        ^^^^^^^ Cannot call \`x.unshift\` because property \`unshift\` is missing in \`$ReadOnlyArray\` [1].
           References:
             3: function foo(x: [1,2]): number { return x.unshift(); }
                                ^^^^^ [1]
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
                                                           ^ Cannot return \`x\` because tuple type [1] has an arity of 2 but tuple type [2] has an arity of 1.
            References:
              4:       function foo(x: [1, 2]): [1] { return x; }
                                       ^^^^^^ [1]
              4:       function foo(x: [1, 2]): [1] { return x; }
                                                ^^^ [2]

          test.js:5
            5:       function bar(x: [1]): [1, 2] { return x; }
                                                           ^ Cannot return \`x\` because tuple type [1] has an arity of 1 but tuple type [2] has an arity of 2.
            References:
              5:       function bar(x: [1]): [1, 2] { return x; }
                                       ^^^ [1]
              5:       function bar(x: [1]): [1, 2] { return x; }
                                             ^^^^^^ [2]
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
                      ^^^ Cannot cast \`foo\` to tuple type because tuple type [1] has an arity of 0 but tuple type [2] has an arity of 1.
            References:
              4:       const foo: [] = [];
                                  ^^ [1]
              5:       (foo: [1]);
                             ^^^ [2]
        `,
      ),
  ]),
  test('It is an error to access a tuple out of bounds', [
    addCode('function foo(x: [1,2]): number { return x[2]; }')
      .newErrors(
        `
          test.js:3
            3: function foo(x: [1,2]): number { return x[2]; }
                                                       ^^^^ Cannot get \`x[2]\` because tuple type [1] only has 2 elements, so index 2 is out of bounds.
            References:
              3: function foo(x: [1,2]): number { return x[2]; }
                                 ^^^^^ [1]
        `,
      )
      .because('Out of bounds access causes an error'),
    addCode('function foo(x: [1,2]): number { return x[-1]; }')
      .newErrors(
        `
          test.js:5
            5: function foo(x: [1,2]): number { return x[-1]; }
                                                       ^^^^^ Cannot get \`x[-1]\` because tuple type [1] only has 2 elements, so index -1 is out of bounds.
            References:
              5: function foo(x: [1,2]): number { return x[-1]; }
                                 ^^^^^ [1]
        `,
      ),
  ]),
  test('Unknown key access returns the general type', [
    addCode('function foo(x: [1], y: number): string { return x[y]; }')
      .newErrors(
        `
          test.js:3
            3: function foo(x: [1], y: number): string { return x[y]; }
                                                                ^^^^ Cannot return \`x[y]\` because number literal \`1\` [1] is incompatible with string [2].
            References:
              3: function foo(x: [1], y: number): string { return x[y]; }
                                  ^ [1]
              3: function foo(x: [1], y: number): string { return x[y]; }
                                                  ^^^^^^ [2]
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
                      ^^^ Cannot cast \`arr\` to tuple type because array literal [1] has an arity of 3 but tuple type [2] has an arity of 2.
            References:
              4:       const arr = [1,2,3];
                                   ^^^^^^^ [1]
              9:       (arr: [1,2]);
                             ^^^^^ [2]

          test.js:10
           10:       (arr: [1,2,3,4]);
                      ^^^ Cannot cast \`arr\` to tuple type because array literal [1] has an arity of 3 but tuple type [2] has an arity of 4.
            References:
              4:       const arr = [1,2,3];
                                   ^^^^^^^ [1]
             10:       (arr: [1,2,3,4]);
                             ^^^^^^^^^ [2]
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
                                                                                 ^^^ Cannot return \`arr\` because array type [1] has an unknown number of elements, so is incompatible with tuple type [2].
            References:
              4:       function foo(arr: Array<number>): [number, number] { return arr; }
                                         ^^^^^^^^^^^^^ [1]
              4:       function foo(arr: Array<number>): [number, number] { return arr; }
                                                         ^^^^^^^^^^^^^^^^ [2]
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
                                                                      ^^^ Cannot return \`arr\` because tuple type [1] is incompatible with array type [2].
            References:
              4:       function foo(arr: [1,2]): Array<number> { return arr; }
                                         ^^^^^ [1]
              4:       function foo(arr: [1,2]): Array<number> { return arr; }
                                                 ^^^^^^^^^^^^^ [2]
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
                      ^ Cannot cast \`a\` to number literal \`10\` because number literal \`1\` [1] is incompatible with number literal \`10\` [2].
            References:
              4:       const tup: [1,2,3,4] = [1,2,3,4];
                                   ^ [1]
              6:       (a: 10);
                           ^^ [2]

          test.js:7
            7:       (b: 20);
                      ^ Cannot cast \`b\` to number literal \`20\` because number literal \`2\` [1] is incompatible with number literal \`20\` [2].
            References:
              4:       const tup: [1,2,3,4] = [1,2,3,4];
                                     ^ [1]
              7:       (b: 20);
                           ^^ [2]

          test.js:8
            8:       (rest: [3,40]);
                      ^^^^ Cannot cast \`rest\` to tuple type because number literal \`4\` [1] is incompatible with number literal \`40\` [2] in index 1.
            References:
              4:       const tup: [1,2,3,4] = [1,2,3,4];
                                         ^ [1]
              8:       (rest: [3,40]);
                                 ^^ [2]
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
                              ^^^ Cannot return \`arr\` because string [1] is incompatible with number literal \`1\` [2] in index 0.
            References:
              4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                                                               ^^^^^^ [1]
              4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                                                                          ^ [2]

          test.js:5
            5:         return arr;
                              ^^^ Cannot return \`arr\` because string [1] is incompatible with number literal \`2\` [2] in index 1.
            References:
              4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                                                               ^^^^^^ [1]
              4:       function foo(arr: $TupleMap<[number, number], number => string>): [1, 2] {
                                                                                             ^ [2]
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
                              ^^^ Cannot return \`arr\` because number [1] is incompatible with string [2] in index 0.
            References:
              4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                          ^^^^^^ [1]
              4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                                                                                  ^^^^^^ [2]

          test.js:5
            5:         return arr;
                              ^^^ Cannot return \`arr\` because number [1] is incompatible with string [2] in index 1.
            References:
              4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                                  ^^^^^^ [1]
              4:       function foo(arr: [number, number]): $TupleMap<[number, number], number => string> {
                                                                                                  ^^^^^^ [2]
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
                              ^^^ Cannot return \`tup\` because number literal \`1\` [1] is incompatible with number [2] in index 0.
            References:
              4:       function foo(tup: [1, 2]): [number, number] {
                                          ^ [1]
              4:       function foo(tup: [1, 2]): [number, number] {
                                                   ^^^^^^ [2]

          test.js:5
            5:         return tup;
                              ^^^ Cannot return \`tup\` because number literal \`2\` [1] is incompatible with number [2] in index 1.
            References:
              4:       function foo(tup: [1, 2]): [number, number] {
                                             ^ [1]
              4:       function foo(tup: [1, 2]): [number, number] {
                                                           ^^^^^^ [2]
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
                                ^^^^^^ Cannot get \`tup[3]\` because tuple type [1] only has 2 elements, so index 3 is out of bounds.
            References:
              4:       function foo(tup: ?[number, number]): number {
                                          ^^^^^^^^^^^^^^^^ [1]
        `,
      ),
  ]),
]);
