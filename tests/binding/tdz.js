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
// while let is in TDZ.
// - allow forward refs to lets from type positions.
//
// we're wrong in two ways, currently:
// - from value positions, we currently enforce TDZ only in-scope.
// this is unsound - a let.const may remain uninitialized when a
// lambda runs. This requires an analysis pass we don't yet have.
// TODO
//

function f0() {
  var v = x * c;  // errors, let + const referenced before decl
  let x = 0;
  const c = 0;
}

function f2() {
  {
    var v = x * c; // errors, let + const referenced before decl
  }
  let x = 0;
  const c = 0;
}

// functions are let-scoped and hoisted
function f3() {
  var s: string = foo();          // ok, finds hoisted outer
  {
    var n: number = foo();        // ok, finds hoisted inner
    function foo() { return 0; }
  }
  var s2: string = foo();         // ok, hoisted outer not clobbered
  function foo() { return ""; }
}

// out-of-scope TDZ not enforced. sometimes right...
function f3() {
  function g() { return x + c; }  // ok, g doesn't run in TDZ
  let x = 0;
  const c = 0;
}

// ...sometimes wrong
function f4() {
  function g() { return x; }
  g();          // should error, but doesn't currently
  let x = 0;
  const c = 0;
}

// - from type positions, we currently error on forward refs to any
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
