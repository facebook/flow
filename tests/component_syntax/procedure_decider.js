// Functions only require return types if they actually return something. We should be sure that
// the logic that looks for returns does not count returns inside nested components.
function bar(): number { return 3 }

export function Procedure() {
  component Foo() {
    return bar();
  }
}
