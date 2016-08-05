/* @flow */

import {suite, test} from '../../tsrc/test/Tester';

export default suite(({addFile, addFiles, addCode}) => [
  test('exact type tests', [
    addCode(`
type ArityError = $Exact<number, number>; // error, 2 params expected 1
`)
      .newErrors(
        `
          test.js:4
            4: type ArityError = \$Exact<number, number>; // error, 2 params expected 1
                                 ^^^^^^^^^^^^^^^^^^^^^^ Incorrect number of type parameters (expected 1)
        `,
      ),

    addFile('a.js')
      .newErrors(
        `
          a.js:44
           44: takesExactlyPerson(subtypeOfPerson); // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           44: takesExactlyPerson(subtypeOfPerson); // error
                                  ^^^^^^^^^^^^^^^ property \`first\`. Property not found in
           38: declare function takesExactlyPerson(person: \$Exact<Person>): void;
                                                                  ^^^^^^ object type

          a.js:56
           56: takesExactlyPerson(returnsSubtypeOfPerson());  // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           56: takesExactlyPerson(returnsSubtypeOfPerson());  // error
                                  ^^^^^^^^^^^^^^^^^^^^^^^^ object type. Inexact type is incompatible with exact type
           38: declare function takesExactlyPerson(person: \$Exact<Person>): void;
                                                           ^^^^^^^^^^^^^^ exact type: Person

          a.js:67
           67: takesExactlyPerson(returnsExactlyPerson2()); // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           67: takesExactlyPerson(returnsExactlyPerson2()); // error
                                  ^^^^^^^^^^^^^^^^^^^^^^^ property \`first\`. Property not found in
           38: declare function takesExactlyPerson(person: \$Exact<Person>): void;
                                                                  ^^^^^^ object type

          a.js:81
           81: takesSubtypeOfPerson2(returnsExactlyPerson()); // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           72: declare function takesSubtypeOfPerson2(person2: Person2): void;
                                                               ^^^^^^^ property \`first\`. Property not found in
           81: takesSubtypeOfPerson2(returnsExactlyPerson()); // error
                                     ^^^^^^^^^^^^^^^^^^^^^^ object type

          a.js:90
           90: takesPersonPred(returnsExactlyPersonPred()); // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           90: takesPersonPred(returnsExactlyPersonPred()); // error
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^ exact type. Unsupported exact type
           87: declare function returnsExactlyPersonPred(): \$Exact<PersonPred>;
                                                                   ^^^^^^^^^^ function type
        `,
      ),

      addFile('b.js')
        .newErrors(
          `
            b.js:23
             23:   return {
                          ^ property \`aliaseses\`. Property not found in
              6: export type Flag = \$Exact<{
                                           ^ object type
          `,
        )

  ]),

]);
