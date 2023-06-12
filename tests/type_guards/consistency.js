// These cases test that the body of the function indeed respects the defined type guard.

class A {}
class B extends A {}
class M {}

function no_return(x: mixed): x is number {} // error no return

function return_true(x: mixed): x is number {
    return true; // error x: mixed ~> number
}

function return_true_in_branch(x: mixed): x is number {
  if (0 < 1) {
    return true; // error in this branch x: mixed ~> number
  } else {
    return typeof x === 'number';
  }
}

function negation_on_union(x: number | string | boolean): x is boolean {
  return (typeof x !== 'number' && typeof x !== 'string'); // okay
}

function always_false_error(x: mixed): x is A {
  return false; // error (technically this should hold but that's not what we do in other refinements)
}

function return_false_under_condition(x: number | string | boolean): x is boolean {
  if (typeof x === 'number') {
    return false; // error (for the same reason as above)
  } else if (typeof x ==='string') {
    return false; // error (for the same reason as above)
  } else {
    return true; // okay
  }
}

const arrow_okay = (x: mixed): x is number => (
  typeof x === 'number' // okay
);

const arrow_error = (x: mixed): x is number => (
  typeof x === 'number' ||
  typeof x === 'string' // error x: string ~> number
);

class C {
  m(x: mixed): x is number {
    return typeof x === 'string'; // error x: string ~> number
  }
}

function null_checking() {
  const tes1 = (x: ?number): x is string => ( // error on string because string ~/> ?number
    x != null // error number ~> string
  );

  const test2 = (x: ?number): x is number => (
    x !== null // error null | undefined ~> string
  );

  const test3 = (x: ?number): x is number => (
    x != null // okay
  );
}

function contextual() {
  declare var arr: Array<?number>;

  arr.filter((x): x is string => ( // error on string because string ~/> ?number
    x != null // error number ~> string
  ));

  arr.filter((x): x is number => (
    x !== null // error null | undefined ~> string
  ));

  arr.filter((x): x is number => (
    x != null // okay
  ));
}


function instanceof_ok(x: mixed): x is A {
  return x instanceof B; // okay
}

function instanceof_error(x: mixed): x is B {
  return x instanceof A; // error A ~> B
}

function nested_functions() {
  function outer(x: mixed): x is A {
    function inner(x: mixed): x is C {
      return x instanceof C; // okay
    };
    return x instanceof A; // okay
  }
}

// This is okay because $NonMaybeType<X> is considered a subtype of X
function non_maybe_poly_1<X>(x: X): x is $NonMaybeType<X> {
  return x != null;
}

function non_maybe_poly_2<X>(x: ?X): x is X { // okay
  return x != null;
}

function is_array(x: mixed): x is $ReadOnlyArray<mixed> { // okay
  return Array.isArray(x);
}

function is_array_poly_error<A>(x: mixed): x is $ReadOnlyArray<A> { // error ROArray<mixed> ~> ROArray<A>
  return Array.isArray(x);
}

function pipe_result() {
  declare function isNumberOrString(x: mixed): x is (number | string);
  declare function isString(x: number | string): x is string;
  declare function isBoolean(x: mixed): x is boolean;

  function okay(x: mixed): x is string {
    return isNumberOrString(x) && isString(x); // okay
  }

  function error(x: mixed): x is string {
    return isNumberOrString(x) && isBoolean(x); // error. For now this check is equivalent to: (number | string) & boolean ~> string
  }
}


//
// Writes
//

function error_write_to_type_guard_param_1(x: mixed): x is A {
  x = 1;
  return x instanceof A; // error 'x' is written to
}

function error_write_to_type_guard_param_2(x: mixed): x is B {
  x = new B();
  return x instanceof B; // error 'x' is written to
}

function error_write_to_type_guard_param_multi(x: mixed): x is B {
  if (0 < 1) {
    x = new B();
    return x instanceof B; // error 'x' is written to (1st loc)
  } else {
    x = new B();
    return x instanceof B; // error 'x' is written to (2nd loc)
  }
}

function multi_return_ok(x: mixed): x is A {
  if (0 < 1) {
    return x instanceof A; // okay
  }
  return x instanceof B; // okay
}

function multi_return_one_branch_error(x: mixed): x is B {
  if (0 < 1) {
    return x instanceof A; // error A ~> B
  }
  return x instanceof B; // okay
}

//
// Havoc
//

function havoc_error(x: mixed): x is B { // error 'x' is havoced
  function y() {
    x = 1;
  };
  y();
  return x instanceof B;
}

function havoc_ok_no_call(x: mixed): x is B {
  function y() {
    x = 1;
  };
  return x instanceof B; // okay 'y' is not called
}

function havoc_ok_new_var(x: mixed): x is B {
  function y() {
    let x = 2;
    x = 1;
  };
  y();
  return x instanceof B; // okay local (to 'y') 'x' is written to
}
