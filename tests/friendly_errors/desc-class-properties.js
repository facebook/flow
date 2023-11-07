/**
 * @format
 * @flow
 */

declare class Y {
  a: string;
}

class X extends Y {
  b: string;
  // prettier-ignore
  #c: string;

  foo() {
    this.a as number; // Error: string ~> number
    super.a as number; // Error: string ~> number
    this.b as number; // Error: string ~> number
    this.#c as number; // Error: string ~> number
  }
}
