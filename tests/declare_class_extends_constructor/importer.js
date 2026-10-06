const {ErrorSubclass, MapSubclass} = require('./exporter');

// Statics and the constructor are inherited from the constructor value.
ErrorSubclass.captureStackTrace({}); // ok
new ErrorSubclass('message').message as string; // ok
MapSubclass.groupBy([1], (x: number) => x); // ok
new MapSubclass<string, number>().set('a', 1); // ok

ErrorSubclass.nope; // error: not a static of `ErrorConstructor`
