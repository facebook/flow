function C(this: {a: number}, a: number) {
  this.a = a;
}

const c = new C(1); // ERROR

// `any` is allowed
declare var A: any;
const a = new A(1); // OK
