// @flow

declare function f1(x: mixed): boolean;

declare function f3(x: mixed): boolean %checks (x !== null);

declare function f4(x: mixed): boolean %checks (x !== null);

function f7(x: mixed):  %checks { return x !== null }

var a0 = (x: mixed) => x !== null;

var a1 = (x: mixed): %checks => x !== null;

(x: mixed): %checks => x !== null;

const insert_a_really_big_predicated_arrow_function_name_here = (x: mixed)
  : %checks => x !== null;

declare var x: empty;
(x)
// $FlowExpectedError[missing-local-annot]: Doesn't make sense to annotate since this is a parsing test.
checks => 123;

type checks = any;

declare function f(x: mixed): checks
(typeof x === null);
