function foo() {}
const args = [3, 4];

foo(1, 2); // error
foo(
  1, // error
  2,
);
foo(...args); // error

foo.call(null, 1, 2); // ok, targ A is inferred from arguments
foo.call(null, ...args); // ok, targ A is inferred from arguments
foo.call(null, 1, 2, ...args); // ok, targ A is inferred from arguments

foo.apply(null, [1, 2]); // ok, targ A is inferred from arguments
foo.apply(null, args); // ok, targ A is inferred from arguments

foo.bind(null, 1, 2); // error
foo.bind(null, ...args); // error
foo.bind(null, 1, 2, ...args); // error
