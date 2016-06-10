

class C {
  constructor(n: number) {}
}
let c = new C(1);

class D extends C {
  constructor(s: string) { super(2); }
}
let d = new D("two");

function fn(): Newable<typeof C> {
  return C; //ok
}
function gn(): Class<Newable<C>> {
  return C; //ok
}
function hn(): Newable<Class<C>> {
  return C; //ok
}

function in_(): Newable<typeof C> {
  return D; //ng
}
function jn(): Class<Newable<C>> {
  return D; //ng //error gets blocked by 24
}
function kn(): Newable<Class<C>> {
  return D; //ng //error gets blocked by 24 & 27
}

function ln(): typeof C {
  return D; //ok
}
function mn(): Class<C> {
  return D; //ok
}
