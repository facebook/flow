let obj: {} = {};
let dict: {foo: string} = {foo: 'bar'};
let promise = Promise.resolve();

let mixedArray: Array<mixed> = [];

let wr1 = new WeakRef(obj); // OK
let wr2 = new WeakRef(dict); // OK
let wr3 = new WeakRef(promise); // OK
let wr4 = new WeakRef(mixedArray); // OK

wr1.deref() as void | typeof obj; // OK
wr2.deref() as void | typeof dict; // OK
wr3.deref() as void | typeof promise; // OK
wr4.deref() as void | typeof mixedArray; // OK

let wr5 = new WeakRef(true); // error, must be objects
let wr6 = new WeakRef(123); // error, must be objects
let wr7 = new WeakRef('not an object'); // error, must be objects

function* generator(): Iterable<{foo: string}> {
  while (true) {
    yield {foo: 'bar'};
  }
}

let wr8 = new WeakRef(generator()); // OK
