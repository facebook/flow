//@flow

/**
 * declaration of foo
 */
declare var foo;

  foo;
// ^

/**
 * declaration of Bar
 */
declare class Bar {};

new Bar();
//   ^

/**
 * declaration of baz
 */
declare function baz(any) : any;

  baz();
// ^
