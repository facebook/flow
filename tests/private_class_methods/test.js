//@flow

class CanDefinePrivateMethod {
  #p: number
  constructor() {this.#p = 3}

  #privateMethod() {
    this.#p = 4;
  }
}

class CanCallPrivateMethods {
  #foo(a: number): number { return a; }
  static #bar(b: string): string { return b; }

  #privateTest(
    other: CanCallPrivateMethods,
    staticOther: typeof CanCallPrivateMethods,
    invalidOther: CanDefinePrivateMethod,
  ): void {
    const fooValid: number = this.#foo(3);
    const otherFooValid: number = other.#foo(4);
    const fooInvalid: string = this.#foo(""); // Error: type mismatch
    const barValid: string = CanCallPrivateMethods.#bar("str");
    const otherBarValid: string = staticOther.#bar("str");
    const barInvalid: number = CanCallPrivateMethods.#bar(3); // Error: type mismatch

    invalidOther.#foo(3); // Error: missing prop #foo
  }

  test(): void {
    this.#privateTest(new CanCallPrivateMethods(), CanCallPrivateMethods, new CanDefinePrivateMethod());
  }
}

class Outer {
  #foo(): number { return 1; }
  #bar(): number { return 1; }

  constructor() {
    const outer: number = this.#foo();
    class Inner {
      #foo(): string { return '1'; }

      test(): void {
        const inner: string = this.#foo();
        this.#bar(); // Error: bar not callable in inner
      }
    }
  }
}

class PrivateMethodUnbound {
  #foo(): number { return 3; }
  static #bar(): string { return 'bar'; }

  test(): void {
    // Method unbound errors
    this.#foo;
    PrivateMethodUnbound.#bar;
  }
}

class C<+T> {
  #private(): this {
      return this;
  }

  #private_contra(x: T) { } // error, polarity

  public(): this {
      declare const c: C<string>;
      declare const c2: C<number>;
      let x = c.#private();
      x = c2.#private(); // error

      return this.#private();
  }
}

declare const c: C<string>;
declare const c2: C<number>;

let x = c.public();
x = c2.public(); // error
