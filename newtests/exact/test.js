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

    addFile('exact_basics.js').newErrors(
                                `
                                  exact_basics.js:48
                                   48: takesExactlyPerson(subtypeOfPerson); // error
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
                                   48: takesExactlyPerson(subtypeOfPerson); // error
                                                          ^^^^^^^^^^^^^^^ property \`first\`. Property not found in
                                   42: declare function takesExactlyPerson(person: \$Exact<Person>): void;
                                                                                          ^^^^^^ object type

                                  exact_basics.js:60
                                   60: takesExactlyPerson(returnsSubtypeOfPerson());  // error
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
                                   60: takesExactlyPerson(returnsSubtypeOfPerson());  // error
                                                          ^^^^^^^^^^^^^^^^^^^^^^^^ object type. Inexact type is incompatible with exact type
                                   42: declare function takesExactlyPerson(person: \$Exact<Person>): void;
                                                                                   ^^^^^^^^^^^^^^ exact type: Person

                                  exact_basics.js:71
                                   71: takesExactlyPerson(returnsExactlyPerson2()); // error
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
                                   71: takesExactlyPerson(returnsExactlyPerson2()); // error
                                                          ^^^^^^^^^^^^^^^^^^^^^^^ property \`first\`. Property not found in
                                   42: declare function takesExactlyPerson(person: \$Exact<Person>): void;
                                                                                          ^^^^^^ object type

                                  exact_basics.js:85
                                   85: takesSubtypeOfPerson2(returnsExactlyPerson()); // error
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
                                   76: declare function takesSubtypeOfPerson2(person2: Person2): void;
                                                                                       ^^^^^^^ property \`first\`. Property not found in
                                   85: takesSubtypeOfPerson2(returnsExactlyPerson()); // error
                                                             ^^^^^^^^^^^^^^^^^^^^^^ object type

                                  exact_basics.js:94
                                   94: takesPersonPred(returnsExactlyPersonPred()); // error
                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ function call
                                   94: takesPersonPred(returnsExactlyPersonPred()); // error
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^ exact type. Unsupported exact type
                                   91: declare function returnsExactlyPersonPred(): \$Exact<PersonPred>;
                                                                                           ^^^^^^^^^^ function type
                                `,
                              ),
    addFile('per_prop_subtyping.js').noNewErrors(),
    addFile('prop_test.js').newErrors(
                             `
                               prop_test.js:13
                                13:   if (p.xxx) {     // error, prop existence test on inexact type
                                            ^^^ property \`xxx\`. Property not found in
                                13:   if (p.xxx) {     // error, prop existence test on inexact type
                                          ^ object type

                               prop_test.js:33
                                33:   if (pc.first) {       // error, prop existence test on union of inexact types
                                             ^^^^^ property \`first\`. Property not found in
                                33:   if (pc.first) {       // error, prop existence test on union of inexact types
                                          ^^ object type

                               prop_test.js:34
                                34:     return pc.last;     // error, last not found on Address
                                                  ^^^^ property \`last\`. Property not found in
                                34:     return pc.last;     // error, last not found on Address
                                               ^^ object type

                               prop_test.js:36
                                36:   return pc.state;      // error, state not found on Person
                                                ^^^^^ property \`state\`. Property not found in
                                36:   return pc.state;      // error, state not found on Person
                                             ^^ object type

                               prop_test.js:43
                                43:   return pc.state;      // error, since (pc: \$Exact<Person>).first may be ""
                                                ^^^^^ property \`state\`. Property not found in
                                43:   return pc.state;      // error, since (pc: \$Exact<Person>).first may be ""
                                             ^^ object type
                             `,
                           ),
    addFile('prop_test2.js').newErrors(
                              `
                                prop_test2.js:36
                                 36:   if (flag.default) {    // error, prop not found (BoolFlag)
                                                ^^^^^^^ property \`default\`. Property not found in
                                 36:   if (flag.default) {    // error, prop not found (BoolFlag)
                                           ^^^^ object type
                              `,
                            ),
    addFile('unsealed.js').newErrors(
                            `
                              unsealed.js:14
                               14: f(o); // error: o incompatible with exact type
                                   ^^^^ function call
                               14: f(o); // error: o incompatible with exact type
                                     ^ object literal. This type is incompatible with
                                4: function f(o: {p: string} | \$Exact<{}>): string {
                                                 ^^^^^^^^^^^^^^^^^^^^^^^^ union: object type | exact type: object type
                                Member 1:
                                  4: function f(o: {p: string} | \$Exact<{}>): string {
                                                   ^^^^^^^^^^^ object type
                                Error:
                                  4: function f(o: {p: string} | \$Exact<{}>): string {
                                                   ^^^^^^^^^^^ property \`p\`. Property not found in
                                 14: f(o); // error: o incompatible with exact type
                                       ^ object literal
                                Member 2:
                                  4: function f(o: {p: string} | \$Exact<{}>): string {
                                                                 ^^^^^^^^^^ exact type: object type
                                Error:
                                 14: f(o); // error: o incompatible with exact type
                                       ^ object literal. Inexact type is incompatible with exact type
                                  4: function f(o: {p: string} | \$Exact<{}>): string {
                                                                 ^^^^^^^^^^ exact type: object type
                            `,
                          ),
    addFile('unsealed2.js').newErrors(
                             `
                               unsealed2.js:9
                                 9:   g(x);
                                      ^^^^ function call
                                15:     return x.q;
                                               ^^^ string. This type is incompatible with the expected return type of
                                13: function f(): number {
                                                  ^^^^^^ number
                             `,
                           ),
  ]),
]);
