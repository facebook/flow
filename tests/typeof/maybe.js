function f<A>(a: A): ?A {
  if (true) {
    return null;
  } else {
    return a;
  }
}
var x = f(42);
'foo' as typeof x; // error: string ~> number
