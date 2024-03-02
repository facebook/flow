class A {
  m(): void {}
}


{
  declare const m: A['m']; // OK - this does not actually unbind anything at runtime
  m as A['m']; // OK
}
