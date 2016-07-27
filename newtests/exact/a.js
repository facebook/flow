/**
 * Test $Exact on object types.
 *
 * @flow
 */

/*
* Exact uppers:

- { x, y } ~> Exact<{x, y}>       // ok
- { x, y, z } ~> Exact<{ x, y }>  // error

- Exact<X> ~> Exact<X>            // ok
- X ~> Exact<X>                   // error

- Exact<X> ~> Exact<Y>            // error

* Exact lowers:

- Exact<{x, y}> ~> { x, y }       // ok
- Exact<{x, y, z}> ~> { x, y }    // ok
- Exact<{x, y}> ~> { x, y, z }    // error

* Unsupported kinds

- Exact<unsupported> ~> X
- X ~> Exact<unsupported>
*/

// values that are exactly a Person have only these properties.
// subtypes may have additional properties.
//
type Person = { salutation: string, last: string };

// object literals have exact inferred types.
//
var exactlyPerson = { salutation: "Mr", last: "Dobalina" };
var subtypeOfPerson = { salutation: "Mr", first: "Bob", last: "Dobalina" };

declare function takesExactlyPerson(person: $Exact<Person>): void;

// { x, y } ~> Exact<{x, y}>
takesExactlyPerson(exactlyPerson); // ok

// { x, y, z } ~> Exact<{ x, y }>
takesExactlyPerson(subtypeOfPerson); // error

// annotated types are more subtle, since they carry their subtypes
// around silently unless they're explicitly exact
//
declare function returnsSubtypeOfPerson(): Person;
declare function returnsExactlyPerson(): $Exact<Person>;

// Exact<X> ~> Exact<X>
takesExactlyPerson(returnsExactlyPerson());  // ok

// X ~> Exact<X>
takesExactlyPerson(returnsSubtypeOfPerson());  // error

// exact types must match, of course
//
type Person2 = { salutation: string, first: string, last: string };

// (let's sample test the type alias code path)
type ExactPerson2 = $Exact<Person2>;
declare function returnsExactlyPerson2(): ExactPerson2;

// Exact<X> ~> Exact<Y>
takesExactlyPerson(returnsExactlyPerson2()); // error

// exact LBs are compatible wherever their inexact counterparts are
//
declare function takesSubtypeOfPerson(person: Person): void;
declare function takesSubtypeOfPerson2(person2: Person2): void;

// Exact<{x, y}> ~> { x, y }
takesSubtypeOfPerson(returnsExactlyPerson()); // ok

// Exact<{x, y, z}> ~> { x, y }
takesSubtypeOfPerson(returnsExactlyPerson2()); // ok

// Exact<{x, y}> ~> { x, y, z }
takesSubtypeOfPerson2(returnsExactlyPerson()); // error

// right now, only exact object types are supported.
//
type PersonPred = (person: Person) => bool;

declare function returnsExactlyPersonPred(): $Exact<PersonPred>;
declare function takesPersonPred(pred: PersonPred): void;

takesPersonPred(returnsExactlyPersonPred()); // error
