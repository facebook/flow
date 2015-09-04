/** @flow */

/* TODO add tests for explicit let/const.
   we use class as proxy for let for now
 */

// -- types ---

// type aliases are hoisted and always available

type T1 = T2;   // ok
type T2 = number;

// --- lets ---

// to be correct, we would
// - not allow forward refs to lets from value positions,
// while let was in TDZ.
// - allow forward refs to lets from type positions.
//
// we're wrong in two ways, currently:
// - for value positions, we currently enforce TDZ only in-scope.
// this is unsound - a let may remain uninitialized when a lambda runs -
// but since classes are the only available lets currently, and they
// used to be vars, we're stricly less unsound than we were. :)
// however, conservative TDZ needs to be
// in place before let/const becomes available per se.
// - for type positions, we currently error on forward refs to any
// value (i.e., class or function). this is a basic flaw in our
// phasing of AST traversal, and will be fixed.
//

var x: C;       // error: C not found (bug)

var y = new C(); // error: let ref before decl from value position

class C {}

var z: C = new C(); // ok

// --- vars ---

// it should be possible to annotate a var with a non-maybe
// type but leave it uninitialized until later.

// the same flow analysis that will enable full TDZ for let/const
// should be usable in a variation to track var lifecycles.

var a: number;  // error now, but shouldn't be

function f(n: number) { return n; }

f(a); // ok now (given above error and annotation) but shouldn't be

a = 10;

f(a); // ok, a: number (not ?number)
