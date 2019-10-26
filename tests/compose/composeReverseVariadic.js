// @flow

declare var pipeVariadic: $ComposeReverseVariadic;

declare var fns1: Array<(number) => number>;
declare var fns2: Array<(number) => string>;
declare var fns3: Array<(number, number) => string>;

declare var inc: number => number;
declare var add: (number, number) => number;
declare var toString: number => string;

(pipeVariadic(inc)(0): empty); // Error: number ~> empty

pipeVariadic(inc)(
  "" // Error: string ~> number
);

(pipeVariadic(inc, toString)(10): empty); // Error: string ~> empty

pipeVariadic(inc, toString)(
  "" // Error: string ~> number
);

pipeVariadic(
  toString,
  inc // Error: string ~> number
)(9);

(pipeVariadic(inc): string => number); // Error: string ~> number
(pipeVariadic(inc): number => string); // Error: number ~> string
((pipeVariadic(inc): number => number): empty); // Error: (number => number) ~> empty

pipeVariadic(); // Error: pipeVariadic min_arity = 1

pipeVariadic(
  inc,
  toString,
  inc // Error: string ~> number
);

pipeVariadic(inc)(1, 2); // Error: No more than 1 argument is expected

(pipeVariadic(add)(1, 2): empty); // Error: number ~> empty

pipeVariadic(add)(1, ""); // Error: string ~> number

pipeVariadic(add)(1, ""); // Error: string ~> number

(pipeVariadic(add, ...fns1): empty); // Error: ((number, number) => number) ~> empty

(pipeVariadic(inc, ...fns1)(1): empty); // Error: number ~> empty

pipeVariadic(
  ...fns2 // Error: string ~> number
);

pipeVariadic(
  ...fns3 // Error: Requires another argument
);

pipeVariadic(inc, ...fns1)(
  1,
  2 // Error: No more than 1 argument is expected
);
