declare class A {
  abstract static create(): this;
  abstract static s(): string;
  abstract clone(): this;
  abstract n(): number;
  concreteClone(): this;
}
let a = new A; //ng

declare class Partial extends A {
  static create(): this;
  clone(): this;
}
let partial = new Partial; //ng

declare class B extends A {
  static create(): this;
  static s(): string;
  clone(): this;
  n(): number;
}
let b = new B;
