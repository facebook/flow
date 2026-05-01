declare function promiseAllByKey<O>(o: O): Promise<{[K in keyof O]: O[K] extends Promise<infer V> ? V : O[K]}>;

promiseAllByKey({
  foo: Promise.resolve(0),
  bar: 'bar',
}).then(o => {
  o.foo as string; // error, number ~> string
  o.bar as 'bar'; // ok
});
