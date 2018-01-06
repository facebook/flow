// @flow

declare var obj: {a?: {b: ?{c: null | {d: number}}}};

let _ =
  obj.a ?
    (obj.a.b ?
      (obj.a.b.c ?
        (obj.a.b.c.d ? obj.a.b.c.d : null) :
        null) :
      null) :
    null;

let myobj = {
  m(n: string) { return n; },
  n: (k: number) => k
};

myobj.m;
myobj.m("a");
myobj.n;
myobj.n(1);

class Base {
  x: string;
  mm (x: string): string | number {
    return x;
  }
}

class A extends Base {
  mm(x: string): string {
    const s = super.mm(x);
    return (typeof s === "string") ? s : "";
  }

  nn() {
    this.mm("x")
  }
}

let inst_a = new A;
inst_a.mm("x");

interface IA {
  mm(x: string): string;
  mm(x: number): number;
  mf : ((x: number) => number) & ((x: string) => string);
}

declare var i_a: IA;
i_a.mm("x");
i_a.mm(1);

i_a.mf("x");
i_a.mf(1);

declare var any: any;
any.mmm(0);
any.fff = 0;

declare var any_obj: Object;
any_obj.mmm(0);
any_obj.fff = 0;

class GGG<X> {
  xxx: X;
  constructor(x: X) {
    this.xxx = x;
  }
  get_X(): X {
    return this.xxx;
  }
  set_X(x: X): void {
    this.xxx = x;
  }
}

var str_g: GGG<string> = new GGG("");
str_g.xxx = "a";
str_g.set_X("b");
str_g.get_X();

var int_g = new GGG(1);
int_g.xxx = 2;
int_g.set_X(3);
int_g.get_X()

var inst_g = (0 < 1) ? str_g : int_g;
inst_g.set_X((1: any));
