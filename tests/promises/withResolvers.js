const {promise, resolve, reject} = Promise.withResolvers<number>();
promise as Promise<number>; // ok
resolve(1); // ok
resolve(Promise.resolve(1)); // Error (bug): `Promise` is not a subtype of `PromiseLike`
reject(new Error()); // ok
reject(); // ok
resolve('s'); // Error: string ~> number
promise as Promise<string>; // Error: number ~> string

Promise.withResolvers<number>(1); // Error: no arguments expected
