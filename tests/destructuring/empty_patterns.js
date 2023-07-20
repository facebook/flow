function assignment_tests() {
  declare var m: any;
  [] = 1; // ok
  ({} = 1); // ok
  [m.foo] = (1: any); // only unsupported syntax error
  [m.foo, {bar: [m.baz]}] = (1: any); // only unsupported syntax error
  return 1;
}

function fun_param_tests() {
  function f1({}): void {} // error: missing-local-annot
  function g1([]): void {} // error: missing-local-annot
  function f2({}) { return 1 } // error: missing-local-annot
  function g2([]) { return 1 } // error: missing-local-annot
  function f3({}: mixed): void {} // ok
  function g3([]: mixed): void {} // ok
  function f4({}: mixed) { return 1 } // ok
  function g4([]: mixed) { return 1 } // ok
}

function var_decl_tests() {
  const {} = (1: any); // ok
  const [] = (1: any); // ok
}
