function class_test() {
  declare var x: unknown;

  class C {
    m(x: unknown): x is number {
      return typeof x === 'number';
    }

    static s(x: unknown): x is string {
      return typeof x ==='string';
    }

    invalid1(x: unknown): x is number {
      x = 1;
      return typeof x === 'number'; // error param write reaching this return
    }

    invalid2(x: unknown): y is number { // error param missing
      return typeof x === 'number';
    }

    invalid3(x: unknown): x is string {
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
    m(x: unknown): boolean { // error extends non-predicate
      return true;
    }
  }

  class D2 extends C {
    m(x: unknown): x is mixed { // error extends
      return true;
    }
  }
  class D3 extends C {
    m(x: unknown): x is number {
      return true; // error mixed ~> number
    }
  }

  // implements checks

  interface I {
    m(x: unknown): x is number;
    n(x: unknown): x is string;
  }

  class E1 implements I {
    m(x: unknown): x is number {
      return typeof x === 'number';
    }
    n(x: unknown): x is number { // error number ~> string
      return typeof x === 'number';
    }
  }

  interface IP<X> {
    m(x: unknown): x is X;
    n(x: unknown): x is X;
  }

  class E2 implements IP<number> {
    m(x: unknown): x is number {
      return typeof x === 'number';
    }
    n(x: unknown): x is string { // error number ~> string
      return typeof x === 'string';
    }
  }
}

function declare_class_test() {
  declare var x: unknown;

  declare class C {
    m(x: unknown): x is number;
    static s(x: unknown): x is string;
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
    m(x: unknown): boolean { // error extends non-predicate
      return true;
    }
  }

  class D2 extends C {
    m(x: unknown): x is mixed { // error mixed ~> number
      return true; // okay
    }
  }
  class D3 extends C {
    m(x: unknown): x is number {
      return true; // error mixed ~> number
    }
  }
}

function poly_class_test() {
  declare var x: unknown;

  declare class P<X> {
    m(x: unknown): x is X;
  }

  const p = new P<number>();
  if (p.m(x)) {
    (x: number);
    (x: string); // error number ~> string
  }
}
