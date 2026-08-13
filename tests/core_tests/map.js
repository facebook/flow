function* generator(): Iterable<[string, number]> {
  while (true) {
    yield ['foo', 123];
  }
}

function* readonlyGenerator(): Iterable<[readonly key: string, readonly value: number]> {
  while (true) {
    yield ['foo', 123];
  }
}

let tests = [
  // good constructors
  function() {
    let w = new Map<string, string>();
    let x = new Map<string, string>(null);
    let y = new Map([['foo', 123]]);
    let z = new Map(generator());
    let a: Map<string, number> = new Map();
    let b: Map<string, number> = new Map([['foo', 123]]);
    let c: Map<string, number> = new Map(generator());
    let d: Map<string, number> = new Map(readonlyGenerator());
  },

  // bad constructors
  function() {
    let x = new Map(['foo', 123]); // error
    let y: Map<number, string> = new Map([['foo', 123]]); // error
  },

  // get()
  function(x: Map<string, number>) {
    x.get('foo') as boolean; // error, string | void
    x.get(123); // error, wrong key type
  },

  // getOrInsert() and getOrInsertComputed()
  function(x: Map<string, number>) {
    x.getOrInsert('foo', 123) as number;
    x.getOrInsert(123, 123); // error, wrong key type
    x.getOrInsert('foo', 'bar'); // error, wrong value type

    x.getOrInsertComputed('foo', key => {
      key as string;
      return 123;
    }) as number;
    x.getOrInsertComputed(123, () => 123); // error, wrong key type
    x.getOrInsertComputed('foo', () => 'bar'); // error, wrong value type
  },

  // WeakMap getOrInsert() and getOrInsertComputed()
  function(x: WeakMap<{foo: string}, number>, key: {foo: string}) {
    x.getOrInsert(key, 123) as number;
    x.getOrInsert('foo', 123); // error, wrong key type
    x.getOrInsert(key, 'bar'); // error, wrong value type

    x.getOrInsertComputed(key, callbackKey => {
      callbackKey as {foo: string};
      return 123;
    }) as number;
    x.getOrInsertComputed('foo', () => 123); // error, wrong key type
    x.getOrInsertComputed(key, () => 'bar'); // error, wrong value type
  },

  // good WeakMap constructors
  function() {
    let symbolKey = Symbol();
    let x: WeakMap<symbol, string> = new WeakMap([[symbolKey, 'test']]);
  },
];
