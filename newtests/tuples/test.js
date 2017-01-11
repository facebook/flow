/* @flow */


import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('You can use the Array$Tuple functions', [
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
  test('You can\'t use Array functions', [
    addCode(
      `function foo(x: [1,2]): number { return x.unshift(); }`
    ).newErrors(
       `
         test.js:3
           3: function foo(x: [1,2]): number { return x.unshift(); }
                                                        ^^^^^^^ property \`unshift\`. Property not found in
           3: function foo(x: [1,2]): number { return x.unshift(); }
                                                      ^ Array\$Tuple
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
                                                       ^^^^ access of computed property/element. Out of bound access. This tuple has 2 elements and you tried to access index 2 of
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
                                                       ^^^^^ access of computed property/element. Out of bound access. This tuple has 2 elements and you tried to access index -1 of
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
                                                     ^^^^ access of computed property/element. Out of bound access. This tuple has 1 elements and you tried to access index 2 of
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
                                ^ number literal \`1\`. This type is incompatible with the expected return type of
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
          test.js:4
            4:       const tup: [1,2,3,4] = [1,2,3,4];
                                       ^ number literal \`4\`. Expected number literal \`40\`, got \`4\` instead
            8:       (rest: [3,40]);
                               ^^ number literal \`40\`

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
                              ^^^ string. This type is incompatible with
            4:       function foo(arr: \$TupleMap<[number, number], number => string>): [1, 2] {
                                                                                        ^ number literal \`1\`

          test.js:5
            5:         return arr;
                              ^^^ string. This type is incompatible with
            4:       function foo(arr: \$TupleMap<[number, number], number => string>): [1, 2] {
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
          test.js:4
            4:       function foo(arr: [number, number]): \$TupleMap<[number, number], number => string> {
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string. This type is incompatible with
            4:       function foo(arr: [number, number]): \$TupleMap<[number, number], number => string> {
                                        ^^^^^^ number

          test.js:4
            4:       function foo(arr: [number, number]): \$TupleMap<[number, number], number => string> {
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ string. This type is incompatible with
            4:       function foo(arr: [number, number]): \$TupleMap<[number, number], number => string> {
                                                ^^^^^^ number
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
          test.js:4
            4:       function foo(tup: [1, 2]): [number, number] {
                                                 ^^^^^^ number. Expected number literal \`1\`
            4:       function foo(tup: [1, 2]): [number, number] {
                                        ^ number literal \`1\`

          test.js:4
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
                                ^^^^^^ access of computed property/element. Out of bound access. This tuple has 2 elements and you tried to access index 3 of
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
