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
          a.js:46
           46: takesExactlyPerson(subtypeOfPerson); // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           46: takesExactlyPerson(subtypeOfPerson); // error
                                  ^^^^^^^^^^^^^^^ property \`first\`. Property not found in
           40: declare function takesExactlyPerson(person: \$Exact<Person>): void;
                                                                  ^^^^^^ object type

          a.js:58
           58: takesExactlyPerson(returnsSubtypeOfPerson());  // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           58: takesExactlyPerson(returnsSubtypeOfPerson());  // error
                                  ^^^^^^^^^^^^^^^^^^^^^^^^ object type. Inexact type is incompatible with exact type
           40: declare function takesExactlyPerson(person: \$Exact<Person>): void;
                                                           ^^^^^^^^^^^^^^ exact type: Person

          a.js:69
           69: takesExactlyPerson(returnsExactlyPerson2()); // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           69: takesExactlyPerson(returnsExactlyPerson2()); // error
                                  ^^^^^^^^^^^^^^^^^^^^^^^ property \`first\`. Property not found in
           40: declare function takesExactlyPerson(person: \$Exact<Person>): void;
                                                                  ^^^^^^ object type

          a.js:83
           83: takesSubtypeOfPerson2(returnsExactlyPerson()); // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           74: declare function takesSubtypeOfPerson2(person2: Person2): void;
                                                               ^^^^^^^ property \`first\`. Property not found in
           83: takesSubtypeOfPerson2(returnsExactlyPerson()); // error
                                     ^^^^^^^^^^^^^^^^^^^^^^ object type

          a.js:92
           92: takesPersonPred(returnsExactlyPersonPred()); // error
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
           92: takesPersonPred(returnsExactlyPersonPred()); // error
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^ exact type. Unsupported exact type
           89: declare function returnsExactlyPersonPred(): \$Exact<PersonPred>;
                                                                   ^^^^^^^^^^ function type
        `,
      )
  ]),

]);
