// @flow

interface I {}
interface J extends I {} //`I` is a type, not a value derived from an expression

declare class C {}
declare class D extends C {}
interface K extends C {
  constructor(n: number): void;
}

function Fn(X: Class<C>): Class<Newable<K>> {
  return class F_ extends X { //ng (extending non-newable)
    constructor(s: string) {} //ng (non-subtype ctor)
  }
}

function Gn(X: Class<Newable<C>>): Class<Newable<K>> {
  return class G_ extends X {
    constructor(n: number) {}
  }
}

function Hn(X: Class<Newable<K>>): Newable<K> {
  return new X(1);
}
