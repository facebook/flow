// @flow

declare var composeVariadic: $ComposeVariadic;

declare var fns1: Array<(number) => number>;
declare var fns2: Array<(number) => string>;
declare var fns3: Array<(number, number) => string>;

declare var inc: number => number;
declare var add: (number, number) => number;
declare var toString: number => string;

(composeVariadic(inc)(0): empty); // Error: number ~> empty

composeVariadic(inc)(
  "" // Error: string ~> number
);

(composeVariadic(toString, inc)(10): empty); // Error: string ~> empty

composeVariadic(toString, inc)(
  "" // Error: string ~> number
);

composeVariadic(
  inc, // Error: string ~> number
  toString
)(9);

(composeVariadic(inc): string => number); // Error: string ~> number
(composeVariadic(inc): number => string); // Error: number ~> string
((composeVariadic(inc): number => number): empty); // Error: (number => number) ~> empty

composeVariadic(); // Error: composeVariadic min_arity = 1

composeVariadic(
  inc, // Error: string ~> number
  toString,
  inc
);

composeVariadic(inc)(1, 2); // Error: No more than 1 argument is expected

(composeVariadic(add)(1, 2): empty); // Error: number ~> empty

composeVariadic(add)(1, ""); // Error: string ~> number

composeVariadic(add)(1, ""); // Error: string ~> number

composeVariadic(
  add, // Error: require another argument
  ...fns1
);

(composeVariadic(inc, ...fns1)(1): empty); // Error: number ~> empty

composeVariadic(
  ...fns2 // Error: string ~> number
);

composeVariadic(
  ...fns3 // Error: Requires another argument
);

composeVariadic(inc, ...fns1)(
  1,
  2 // Error: No more than 1 argument is expected
);
