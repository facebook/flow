function class_test() {
  declare var x: mixed;

  class C {
    m(x: mixed): x is number {
      return typeof x === 'number';
    }

    static s(x: mixed): x is string {
      return typeof x ==='string';
    }

    invalid1(x: mixed): x is number {
      x = 1;
      return typeof x === 'number'; // error param write reaching this return
    }

    invalid2(x: mixed): y is number { // error param missing
      return typeof x === 'number';
    }

    invalid3(x: mixed): x is string {
      return typeof x === 'number'; // error number ~> string
    }
  }

  const c = new C();
  if (c.m(x)) {
    (x: number);
    (x: string); // error number ~> string
  }
  if (C.s(x)) {
    (x: number); // error string ~> number
    (x: string);
  }

  // extends checks

  class D1 extends C {
    m(x: mixed): boolean { // error extends non-predicate
      return true;
    }
  }

  class D2 extends C {
    m(x: mixed): x is mixed { // error extends
      return true;
    }
  }
  class D3 extends C {
    m(x: mixed): x is number {
      return true; // error mixed ~> number
    }
  }

  // implements checks

  interface I {
    m(x: mixed): x is number;
    n(x: mixed): x is string;
  }

  class E1 implements I {
    m(x: mixed): x is number {
      return typeof x === 'number';
    }
    n(x: mixed): x is number { // error number ~> string
      return typeof x === 'number';
    }
  }

  interface IP<X> {
    m(x: mixed): x is X;
    n(x: mixed): x is X;
  }

  class E2 implements IP<number> {
    m(x: mixed): x is number {
      return typeof x === 'number';
    }
    n(x: mixed): x is string { // error number ~> string
      return typeof x === 'string';
    }
  }
}

function declare_class_test() {
  declare var x: mixed;

  declare class C {
    m(x: mixed): x is number;
    static s(x: mixed): x is string;
  }

  const c = new C();
  if (c.m(x)) {
    (x: number);
    (x: string); // error number ~> string
  }

  if (C.s(x)) {
    (x: number); // error string ~> number
    (x: string);
  }

  class D1 extends C {
    m(x: mixed): boolean { // error extends non-predicate
      return true;
    }
  }

  class D2 extends C {
    m(x: mixed): x is mixed { // error mixed ~> number
      return true;
    }
  }
  class D3 extends C {
    m(x: mixed): x is number {
      return true; // error mixed ~> number
    }
  }
}

function poly_class_test() {
  declare var x: mixed;

  declare class P<X> {
    m(x: mixed): x is X;
  }

  const p = new P<number>();
  if (p.m(x)) {
    (x: number);
    (x: string); // error number ~> string
  }
}
