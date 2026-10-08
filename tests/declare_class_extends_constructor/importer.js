const {
  ErrorSubclass,
  MapSubclass,
  PromiseSubclass,
  PrototypeOnlySubclass,
} = require('./exporter');

// Statics and the constructor are inherited from the constructor value.
ErrorSubclass.captureStackTrace({}); // ok
new ErrorSubclass('message').message as string; // ok
MapSubclass.groupBy([1], (x: number) => x); // ok
new MapSubclass<string, number>().set('a', 1); // ok
PromiseSubclass.resolve(1) as Promise<number>; // ok
new PromiseSubclass<number>(resolve => resolve(1)); // ok

ErrorSubclass.nope; // error: not a static of `ErrorConstructor`

declare const prototypeOnly: PrototypeOnlySubclass;
prototypeOnly.instanceProp as number; // ok: from the `prototype` instance
prototypeOnly.instanceProp as string; // error: number is not string
PrototypeOnlySubclass.staticProp as string; // ok: statics come from the constructor value
