type F<T> = $ReadOnly<{
    log: (() => T) => void,
    ...
  }>;

  declare function map<O: {...}>(o1: O): {[K in keyof O]: O[K] extends F<infer V> ? V : empty};

  const foo = Object.freeze({bar: {log: (f: () => string) => {}}})
  const o1 = map(foo) as {+bar: number}; // error
