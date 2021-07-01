// @flow

enum E {
  A,
  B,
}

///////////
// Valid //
///////////
const a: void | E = E.cast('A');
declare var maybeString: ?string;
E.cast(maybeString);
const b: Iterable<E> = E.members();
const c: boolean = E.isValid('A');
E.isValid(maybeString);
const s: string = E.getName(E.A);

const cast: (string) => void | E = E.cast;
const members: () => Iterable<E> = E.members;
const isValid: (string) => boolean = E.isValid;

////////////
// Errors //
////////////
// Cannot get non-existent method
E.nonExistent; // Error

// Cannot call non-existent method
E.nonExistent(); // Error

// Computed access not allowed
E['members'](); // Error

// Attempt calling an enum member
E.A(); // Error

// Object.prototype is not in the prototype chain
E.toString(); // Error

// `getName` errors
(E.getName(E.B): boolean); // Error - wrong type

// `this` must be the enum object
const o = {
  cast: E.cast,
};
o.cast('x'); // Error
E.cast('x'); // OK
o.cast.call(E, 'x'); // OK
