/* @flow
 * test errors on illegal rebinding/reassignment
 *
 *       type let const var (reassign)
 * type  x    x   x     x   x
 * let   x    x   x     x
 * const x    x   x     x   x
 * var   x    x   x
 *
 * TODO add tests using explicit let/const decls.
 * For now, we use class decls as a proxy for let,
 * and leave const out entirely.
 */

// type x *

function type_type() {
  type A = number;
  type A = number;  // error: name already bound
}

function type_let() {
  type A = number;
  class A {}        // error: name already bound
}

/*function type_const() {
  type A = number;
  const A = 0;     // error: name already bound
}*/

function type_var() {
  type A = number;
  var A = 0;        // error: name already bound
}

function type_reassign() {
  type A = number;
  A = 42;           // error: type alias ref'd from value pos
}
// let x *

function let_type() {
  class A {}
  type A = number;  // error: name already bound
}

function let_let() {
  class A {}
  class A {}        // error: name already bound
}

/*function let_const() {
  class A {}
  const A = 0;     // error: name already bound
}*/

function let_var() {
  class A {}
  var A = 0;        // error: name already bound
}

// const x *
/*
function const_type() {
  const A = 0;
  type A = number;  // error: name already bound
}

function const_let() {
  const A = 0;
  class A {}        // error: name already bound
}

function const_const() {
  const A = 0;
  const A = 0;     // error: name already bound
}

function const_var() {
  const A = 0;
  var A = 0;        // error: name already bound
}

function const_reassign() {
  const A = 0;
  A = 42;           // error: cannot be reassigned
}
*/
// var x *

function var_type() {
  var A = 0;
  type A = number;  // error: name already bound
}

function var_let() {
  var A = 0;
  class A {}        // error: name already bound
}

/*function var_const() {
  var A = 0;
  const A = 0;      // error: name already bound
}*/
