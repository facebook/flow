function f() {
  match (a) {
    1 => return true;
    2 => f();
    3 => throw e;
    4 => while (true) {}
  }
}
