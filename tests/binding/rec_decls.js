function referenced_in_type_annotation_of_declare_var() {
  declare var x1: x1; // error recursive-definition
  declare var x2: typeof x2; // error recursive-definition
  declare var V1: number extends infer V1 ? V1 : empty; // okay
  declare var V2: V2 extends infer V2 ? V2 : empty; // error recursive-definition

  declare var rec_obj_okay: { f: typeof rec_obj_okay }; // okay due to object type constructor
  declare var rec_obj_err:
    | { f: typeof rec_obj_err }
    | typeof rec_obj_err; // error here
}

function referenced_in_type_annotation_of_const() {
  const x1: x1 = 1; // error on x1 not `1`
  const x2: typeof x2 = 1; // error on x2 not `1`
  const x3: typeof x3.f = 1; // error on x2 not `1`
}

function recursion_limit_exceeded_regression_1() {
  declare var Val: Val<string>; // error on Val, no recursion-limit-exceeded
  const {f} = Val;
}

function recursion_limit_exceeded_regression_2() {
  let Val: Val<string>;
  Val = (42: any);
  const {f} = Val; // error on Val, no recursion-limit-exceeded
}

function referenced_in_param_default() {
  function f(x: number = x) {} // error: x references the param being declared
  function g({a}: {a: number} = a) {} // error: a references the param being declared
  function h(y: number = y + 1) {} // error: y references the param being declared
  function i(x: number = x + x) {} // error: x references the param being declared (twice)

  // Pattern-inline defaults referencing earlier params are OK
  function default_expr_scope_ok({a, b = a}: {a: string, b?: string}) {} // okay: b's default references a, which comes before

  // Pattern-inline defaults referencing later params are NOT OK (forward reference)
  function default_expr_scope_err({a = b, b}: {a?: string, b: string}) {} // error: a's default references b, which comes after

  // Multiple destructured params with various reference patterns
  function multi_param_refs(
    {a, b = a, c = c}: {a: string, b?: string, c?: string}, // b = a OK (earlier), c = c ERROR (self)
    {d = a, e = d, f = g, g}: {d?: string, e?: string, f?: string, g: string}, // d = a OK (earlier param), e = d OK (earlier in same pattern), f = g ERROR (forward)
  ) {}

  // These are okay (referencing outer scope)
  const outer = 1;
  function ok1(z: number = outer) {} // okay
  function ok2(w: number = 2) {} // okay
  function ok3(x: any = x => x) {} // okay
  function ok4(x: any = class x { m() {x}}) {} // okay
}

type O = { z?: string };

function Foo({z = z ?? null}: O) { // error: z references the param being declared
  return z;
}

component Comp(...{scope = scope ?? null}: {scope?: ?string}) { // error: scope references the param being declared
  return scope;
}
