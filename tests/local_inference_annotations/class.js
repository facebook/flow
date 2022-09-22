//@flow
class A {
  method(param): any { // error on param
    (x) => 3; // error
    return (x) => 3; // ok
  }

  property1 = (x) => 3; // error
  property2: any = (x) => 3; //ok

  methodBody(annotatedParam: any) { // ok
    const x: any = (x) => 3; // ok
  }

  annotatedMethod(param: any): any {}
}

class B {
  a; // error
  declare b; // error
  c = 1; // ok
  d: number; // ok
  declare e: number; // ok
  f: number = 1; // ok
}

class C{
  prop1 = 42; // ok
  prop2 = 10 + 30; // err
  prop3; // err
  prop4 = (x: number): number => 42; // ok
  prop5 = (x: number) => 42; // err return
  prop6 = function (x: number) { return 42 } // err return

  method1() { } // ok
  method2() { return 42; } // err return
  constructor() { return 10; } // no annotation required, but error on non-void return

  #priv1 = 10 + 20; // err
  #priv2; // err

  #pmeth1() { } // ok
  #pmeth2() { return 42 } // err return
  ["computed"]() { } // unsupported, but no missing annot

  throwingInvariant1() { invariant() } // error: throws
  throwingInvariant2() { invariant(false) } // error: throws
  nonthrowingInvariant() { invariant(this.prop1) } // ok
}

function sanity_check_that_we_dont_error_on_non_method_functions() {
  return 42;
}

declare function invariant(...$ReadOnlyArray<mixed>): void;
