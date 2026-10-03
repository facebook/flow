function refines(x: ?string): string {
  libAssert(x != null);
  return x;
}

function neverReturns(c: boolean): string {
  libAssert(false);
  return "default string";
}
