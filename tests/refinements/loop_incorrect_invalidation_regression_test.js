{
  declare function f(v: string): boolean;

  function test(v: string | null) {
    if (v == null) {
      return;
    }
    while (true) {
      if (f(v)) { // spurious error
        return;
      }
    }
  }
}
