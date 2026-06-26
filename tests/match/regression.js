// Regression test crash
declare function f(x: string): void;
function test(x: 1 | 2 | 3, o: {foo: string}) {
  const a = o.foo || '';
  const b = f("foo");

  match (x) {
    1 => {
      return;
    }
    2 => {
      return;
    }
    3 => {
      return;
    }
  }
}

function test_guard_with_abrupt_body(val: unknown): void {
  match (val) {
    'foo' if (Array.isArray(val)) => { // should not crash
      return;
    }
    _ => {}
  }
}
