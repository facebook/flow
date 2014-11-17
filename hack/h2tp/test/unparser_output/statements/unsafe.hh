<?hh
function foo(string $x): string {
  if ($x === "Hello") { // UNSAFE
    return 5;
  }
  return $x;
}
function bar() { // UNSAFE
  return $y;
}