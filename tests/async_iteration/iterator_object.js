async function* gen(): AsyncGenerator<number, string, boolean> {
  const b: boolean = yield 1;
  return 'done';
}

gen() as AsyncIteratorObject<number, string, boolean>; // OK
gen() as AsyncIteratorObject<number | string, string | void, true>; // ERROR: invariant type parameters
gen() as AsyncIteratorObject<number, number, boolean>; // Error: string ~> number
gen()[Symbol.asyncDispose](); // OK: inherited from AsyncIteratorObject
