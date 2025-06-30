function* f() {
  (match (x) {
    0 => yield 1,
    1 => yield* 1,
  });
}
