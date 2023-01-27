// @flow
class C1 {
  m() { }
  m() { }
}

new C1().m();

class C2 {
  get m(): number { return 42; }
  m() { }
}

new C2().m();

class C3 {
  set m(x: number): void { }
  m() { }
}

new C3().m();

class C4 {
  get m(): number { return 42; }
  set m(x: number): void { }
}

new C4().m = new C4().m - 42;

class C5 {
  m() { }
  get m(): number { return 42; }
  set m(x: number): void { }
}

new C5().m = new C5().m - 42;
