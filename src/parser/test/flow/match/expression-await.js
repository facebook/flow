async function f() {
  return match (x) {
    _ => await x,
  };
}
