// @flow

interface I {}
interface J extends I {} //`I` is a type, not a value derived from an expression

declare class C {
  constructor(s: string): void;
}
declare class D extends C {
  constructor(n: number): void;
}
interface K extends C {
  constructor(n: number): void;
}

function Fn(X: Class<C>): Class<Newable<K>> {
  return class F_ extends X { //ng (extending non-newable)
    constructor(s: string) {} //ng (non-subtype ctor)
  }
}

function Gn(X: Class<Newable<C>>): Class<Newable<K>> {
  return class G_ extends X { //ok
    constructor(n: number) {} //ok
  }
}

function hn(X: Class<Newable<K>>): Newable<K> {
  return new X(1);
}
let h1 = hn(C); //ng
let h2 = hn(D); //ok
let h3 = new h2.constructor(1); //ok
