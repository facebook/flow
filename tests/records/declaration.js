// Empty record
{
  record R {}

  declare const x: R;

  x as R; // OK
  x.xxx; // ERROR: property not found
}

// Basic field access
{
  record R {
    a: number,
  }

  declare const x: R;

  x.a as number; // OK
  x.a as empty; // ERROR: `number`
}

// Multiple fields
{
  record R {
    a: number,
    b: string,
    c: boolean,
  }

  declare const x: R;

  x.a as number; // OK
  x.b as string; // OK
  x.c as boolean; // OK
  x.xxx; // ERROR: property not found
}

// Read-only
{
  record R {
    a: number,
  }

  declare const x: R;

  x.a = 1; // ERROR: not writable
}

// Type alias with records
{
  record R {
    value: number,
  }

  type Alias = R;
  declare const x: Alias;

  x as R; // OK
  x.value as number; // OK
}

// Destructuring
{
  record R {
    a: number,
    b: string,
    c: boolean,
  }

  declare const x: R;

  const {a: renamed} = x;
  renamed as number; // OK

  const {a, ...rest} = x;
  a as number; // OK
  rest.b as string; // OK
  rest.c as boolean; // OK

  const {...asObj} = x;
  asObj as {a: number, b: string, c: boolean, ...}; // OK - TODO: records - make exact
}

// Methods
{
  record R {
    a: number,

    double(): number {
      return this.a ** 2;
    }

    // Referencing other method
    quadruple(): number {
      return this.double() * 2; // OK
    }

    equals(other: R): boolean {
      // Using `this`
      return this.a === this.a; // OK
    }
  }

  declare const x: R;

  x.double() as number; // OK
  x.quadruple() as number; // OK

  declare const y: R;
  x.equals(y) as boolean; // OK
}

// Static methods
{
  record R {
    a: number,

    static sum(x: R, y: R): number {
      return x.a + y.a;
    }
    static random(): R {
      declare const x: R;
      return x;
    }
  }

  declare const x: R;
  declare const y: R;

  R.sum(x, y) as number; // OK
  R.random() as R; // OK
  R.random = () => x; // ERROR: method not writable
}

// Static values
{
  record R {
    static zero: number = 0,
  }

  R.zero as number; // OK
  R.zero as empty; // ERROR: `number`
  R.zero = 1; // ERROR: static not writable
}

// Static methods with generics
{
  record R<T> {
    value: T,

    static create<U>(val: U): R<U> {
      declare const result: R<U>;
      return result;
    }
  }

  R.create(42) as R<number>; // OK
  R.create("test") as R<string>; // OK
}

// Equality
{
  record R {
    a: number,
  }

  declare const x: R;

  if (x === x) {} // OK
  if (x === 1) {} // ERROR
}

// Definitions for below
interface Equatable<T> {
  equals(T): boolean;
}

// Subtyping with interfaces
{
  record R {
    a: number,

    equals(other: R): boolean {
      return this.a === this.a;
    }
  }

  declare const x: R;

  x as Equatable<R>; // OK

  interface A {
    +a: number;
  }
  x as A; // OK

  interface E {
    xxx: number,
  }
  x as E; // ERROR
}

// Subtyping with objects not allowed
{
  record R {
    a: number,
  }

  declare const x: R;

  x as {+a: number}; // ERROR - TODO: records - make exact
  x as {+a: number, ...}; // ERROR - TODO: records - update error message language
}

// Generics and implements
{
  record R<T> implements Equatable<R<T>> {
    foo: string,
    bar: T,
    equals(other: R<T>): boolean {
      return this.foo === other.foo && this.bar === other.bar;
    }
    static yes(): boolean {
      return true;
    }
  }
  declare const x: R<number>;
  declare const y: R<number>;

  x.equals(y) as boolean; // OK
  R.yes() as boolean; // OK
}

// `instanceof` refinement and `match` instance patterns
{
  record RN {
    a: number,
  }

  record RS {
    a: string,
  }

  declare const x: RN | RS;

  if (x instanceof RN) {
    x.a as number; // OK
  } else if (x instanceof RS) {
    x.a as string; // OK
  } else {
    x as empty; // OK
  }

  match (x) {
    RN {const a, ...} => { // TODO: records - make exact
      a as number; // OK
    }
    RS {const a, ...} => { // TODO: records - make exact
      a as string; // OK
    }
  }
}

// `typeof` refinement
{
  record R {
    a: number,
  }

  declare const x: R | void;

  if (typeof x === 'object') {
    x as R; // OK
  } else {
    x as void; // OK
  }
}

// Spread
{
  record R {
    a: number,

    double(): number {
      return this.a ** 2;
    }
  }

  declare const x: R;

  const o: {a: number, ...} = {...x}; // OK - TODO: records - make exact
}

// Record itself
{
  record R {
    a: number,
  }

  const O = {
    r: R,
  };

  declare const x: O.r;

  x.a as number; // OK

  R = null; // ERROR: cannot reassign - TODO: records - update error message language
}

// Multiple generic parameters
{
  record R<T, U> {
    first: T,
    second: U,

    swap(): R<U, T> {
      declare const result: R<U, T>;
      return result;
    }
  }

  declare const x: R<number, string>;

  x.first as number; // OK
  x.second as string; // OK
  x.swap() as R<string, number>; // OK
}

// Multiple implements
{
  interface I1 {
    foo(): string;
  }

  interface I2 {
    bar(): number;
  }

  record R implements I1, I2 {
    foo(): string {
      return "test";
    }
    bar(): number {
      return 42;
    }
  }

  declare const x: R;

  x as I1; // OK
  x as I2; // OK
  x.foo() as string; // OK
  x.bar() as number; // OK
}

// Generic constraints
{
  record R<T: number | string> {
    value: T,
  }

  declare const x: R<number>; // OK
  declare const y: R<string>; // OK
  declare const z: R<boolean>; // ERROR

  x.value as number; // OK
  y.value as string; // OK
}

// Nested record types
{
  record Inner {
    value: number,
  }

  record Outer {
    inner: Inner,
    name: string,
  }

  declare const x: Outer;

  x.inner as Inner; // OK
  x.inner.value as number; // OK
  x.name as string; // OK
}
