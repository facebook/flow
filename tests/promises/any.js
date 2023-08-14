// First argument is required
Promise.any<Array<mixed>>(); // Error: expected array instead of undefined (too few arguments)

// Mis-typed arg
Promise.any<Array<mixed>>(0); // Error: expected array instead of number

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
