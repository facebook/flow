//@flow

/**
 * this is foo!
 */
var foo = 1;

foo = foo;
//     ^

  foo;
// ^

/**
 * this is bar!
 */
var bar = alert(1);

  bar;
// ^

/**
 * this is baz!
 */
const baz = bar;

  baz;
// ^

/**
 * this is alpha!
 */
let alpha = foo;

  alpha;
//  ^

/**
 * this is beta!
 */
var beta;

  beta;
// ^

beta = foo;

  beta;
// ^
