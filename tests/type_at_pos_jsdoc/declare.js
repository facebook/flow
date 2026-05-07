//@flow

/**
 * declaration of foo
 */
declare const foo: empty;

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
declare function baz(x: any) : any;

  baz();
// ^
