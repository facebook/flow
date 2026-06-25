// Exhaustiveness falls out of union narrowing: once all members are handled the
// scrutinee narrows to `never`, just like in TypeScript.
enum Dir {
  Up = 1,
  Down = 2,
}

function f(d: Dir): string {
  switch (d) {
    case Dir.Up:
      return "up";
    case Dir.Down:
      return "down";
    default:
      d satisfies never; // OK: all members handled -> `never`
      return "";
  }
}

function missing(d: Dir): string {
  switch (d) {
    case Dir.Up:
      return "up";
    default:
      d satisfies never; // ERROR: Down (2) not handled
      return "";
  }
}
