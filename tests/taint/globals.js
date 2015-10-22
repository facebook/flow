// @flow

class A {
  f(x : $Tainted<Location>) {
    // Should be an error.
    document.location = x;
  }
  f1(x : $Tainted<Location>) {
    // TODO(rcastano): should cause an error.
    window.location = x;
  }
}
