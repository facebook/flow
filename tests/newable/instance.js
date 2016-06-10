

class C {
  constructor(n: number) {}
}
let c = new C(1);

class D extends C {
  constructor(s: string) { super(2); }
}
let d = new D("two");

function fn(): Newable<typeof c> {
  return new c.constructor(3); //ok
}
function gn(): Newable<C> {
  return new C(4); //ok
}

function hn(): Newable<typeof c> {
  return new D("five"); //ng
}
function in_(): Newable<C> {
  return new d.constructor("six"); //ng //error gets blocked by 21
}

function jn(): typeof c {
  return new D("seven"); //ok
}
function kn(): C {
  return new d.constructor("eight"); //ok
}

