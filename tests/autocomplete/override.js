// @flow

class C {
  override(): number | string { return 0; }
}

class D extends C {
  foo(): string { return this.override() }
  override(): string { return ""; }
  bar() { this.
//             ^
