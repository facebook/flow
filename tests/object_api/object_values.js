Object.entries({a: 1}); // OK

class A {
  a: number;
  constructor(a: number) {
    this.a = a;
  }
}
const a = new A(1);
Object.entries(a); // OK

// Invalid inputs
Object.values(undefined); // ERROR
Object.values(null); // ERROR
Object.values(1); // ERROR
Object.values(true); // ERROR
