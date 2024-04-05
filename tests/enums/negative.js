// Negative numbers
enum N {
  A = -1,
  B = -2,
}

{
  const x = N.A; // OK
  x as N; // OK
  N.cast(-1) as N | void; // OK
}
