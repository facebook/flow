// @flow

class C {
  constructor(n: number) {}
}
class D extends C {
  constructor(s: string) {}
}
export class E extends D {
  constructor(n: number) {}
}

function Fn(X: Class<C>): Class<Newable<C>> {
  return class F_ extends X { //ng (non-newable `X`)
    // The `super(...)` calls required from constructor bodies imply that the
    // extension class should be newable-qualified
    constructor(s: string) {} //ng (non-subtype ctor)
  }
}

function Gn(X: Class<Newable<C>>): Class<C> {
  return class G_ extends X { //ok
    constructor(s: string) {} //ok
  }
}
let G = Gn(C); //ok
let g = new G("one"); //ng

function Hn(X: Class<Newable<C>>): Class<Newable<C>> {
  return class H_ extends X {}
}
let H = Hn(C); //ok
let h = new H(2); //ok

interface I {
  constructor(o: {x:number, y:number}): void
}

export function Ln(X: Class<Newable<C>>): Class<C> & Class<Newable<I>> {
  return class L_ extends X {
    constructor(o: {x:number, y:number}) {}
  }
}
let L = Ln(C); //ok
let ell1 = new L({x:1, y:2, z:3}); //ok
//Under another PR: `let ell2 = new ell1.constructor({x:11, y:12, z:13});` //ok

function Mn(X: Class<Newable<C>>): Class<C> & Class<Newable<I>> {
  return class M_ extends X {
    constructor(n: number) {} //ng
  }
}

function Nn(X) {
  return class N_ extends X {
    constructor(config: {x1:number, x2:string}) {}
    m(): number { return 21; }
  }
}
let N = Nn(C);
let n = new N({x1:"not a number", x2:7});
let a: string = n.m();
