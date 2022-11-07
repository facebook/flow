//@flow

function identity<T>(x: T): T { return x; }
const x: string = identity<_>('string'); // Ok
const y: string = identity<_>(3); // Error, string incompatible with number.

declare function unimplementable<T>(): {x: T};


const a = unimplementable<_>(); // Ok, not exported. Leaks a tvar, errors in LTI and pinned to any

var b: {x: string} = a; // Concretize to string.
(a: {x: string}); // Ok
(a: {x: number}); // Not ok, number incompatible with string, OK in LTI since targ pinned to any

const z = identity<_>(3); // Give z a lower bound.
(z: string); // Error, number lower bound string upper bound

declare function readOnly<T>(): {+x :T};

const result = {
  x: unimplementable<_>(), // Error in LTI, requires concrete annot
  y: readOnly<_>(), // Error in LTI
};
