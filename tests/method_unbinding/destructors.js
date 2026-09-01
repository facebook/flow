class A {
  m(): void {}
}


{
  declare const m: A['m']; // OK - this does not extract anything at runtime
  m as A['m']; // OK
}
