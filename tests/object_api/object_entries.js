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
Object.entries(undefined); // ERROR
Object.entries(null); // ERROR
Object.entries(1); // ERROR
Object.entries(true); // ERROR
