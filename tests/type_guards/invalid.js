class A {}
class B {}
class C extends A {}

class Invalid_type_guard_positions {
  constructor(x: A): x is C { // TODO error no type guard on constructor
    return x instanceof C;
  }
  get p(): this is C {  // TODO error no type guard on getter
    return this instanceof C;
  }
  set q(x: A): x is C {  // TODO error no type guard on setter
    return x instanceof C;
  }
}

function no_return(x: mixed): x is number {}
