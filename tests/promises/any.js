// First argument is required
Promise.any<Array<unknown>>(); // Error: expected array instead of undefined (too few arguments)

// Mis-typed arg
Promise.any<Array<unknown>>(0); // Error: expected array instead of number

// Promise.any supports iterables
function test(val: Iterable<Promise<number>>) {
  const r: Promise<number> = Promise.any(val);
}

function test2(val: Map<string, Promise<number>>) {
  const r: Promise<number> = Promise.any(val.values());
}

function test3(val: Array<Promise<number>>) {
  const r: Promise<number> = Promise.any(val);
}

// Heterogeneous arrays resolve to the union of the awaited element types
function test4(a: Promise<number>, b: Promise<string>) {
  const r: Promise<number | string> = Promise.any([a, b]);
  Promise.any([a, b]) as Promise<number>; // Error: string is incompatible with number
}

function test5() {
  AggregateError([new Error()]) as AggregateError; // ok: calling without `new` returns an `AggregateError`
  new AggregateError([], 'message');
  new AggregateError([], 1); // Error: the message must be a string
}

// As with `Promise.all`, the result is `Awaited<X>`, which doesn't reduce for a generic `X`
function test6<X>(a: Array<X>): Promise<X> {
  return Promise.any(a); // Error
}

function test7<X>(a: Array<Promise<X>>): Promise<X> {
  return Promise.any(a); // ok
}
