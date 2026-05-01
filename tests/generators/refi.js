function *a(x: {a: void | string}): Generator<void, void, void> {
  if (!x.a) return;
  x.a as string; // ok
  yield;
  x.a as string; // error
}

function *b(x: void | string): Generator<void, void, void> {
  if (!x) return;
  x as string; // ok
  yield;
  x as string; // ok
}

declare function fn(): Generator<void, void, void>;

function *c(x: {a: void | string}): Generator<void, void, void> {
  const gen = fn();
  if (!x.a) return;
  x.a as string; // ok
  yield * gen;
  x.a as string; // error
}

function *d(x: void | string): Generator<void, void, void> {
  const gen = fn();
  if (!x) return;
  x as string; // ok
  yield * gen;
  x as string; // ok
}
