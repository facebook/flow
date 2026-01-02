class A {}
class B {}
class C extends A {}

class Invalid_type_guard_positions {
  constructor(x: A): x is C { // error no type guard on constructor
    return x instanceof C;
  }
  get p(): this is C {  // error no type guard on getter
    return this instanceof C;
  }
  set q(x: A): x is C {  // error no type guard on setter
    return x instanceof C;
  }
}

function no_return(x: unknown): x is number {}

function *generator(x: unknown): x is number { // error
  return typeof x == "number";
}

declare var x: unknown;
if (generator(x)) {
  (x: number); // error no refining effect
}

async function async(x: unknown): x is number { // error
  return typeof x == "number"
};

declare class Invalid_type_guard_in_declare_class {
  get p(): x is number;
  set p(x: unknown): x is number;
  constructor(x: unknown): x is number;
}

type Error_in_type_guard_type = (x: unknown) => x is Missing; // error missing name
