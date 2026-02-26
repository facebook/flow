// Functions only require return types if they actually return something. We should be sure that
// the logic that looks for returns does not count returns inside nested components.
function bar(): number { return 3 }

export function Procedure() {
  component Foo() {
    return bar();
  }
}

// Generic functions require return annotations if they have non-void returns.
// A return inside a nested component should not count as a return of the
// enclosing generic function.
function generic<T>() { // OK
  component Bar() {
    return null;
  }
}
