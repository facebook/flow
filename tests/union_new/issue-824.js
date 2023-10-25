import { B, C } from "./issue-824-helper";

type K = B | C;

type I = {
  which(): number;
};

export default class A {
  static foo(p: K): boolean {
    return false;
  }
  static bar(p: I & K): boolean {
    return this.foo(p);
  }
}

/* Simpler repro */
declare var x: boolean & (number | string);
(x: number | string);
