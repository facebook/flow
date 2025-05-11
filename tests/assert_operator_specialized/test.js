declare const arr: Array<number>;
arr[0]!; // ok
declare const roarr: $ReadOnlyArray<number>;
roarr[0]!; // ok
declare const tuple: [?number];
tuple[0]!; // error

declare const exact_obj: { a?: number }
exact_obj.a!; // error
exact_obj['a']!; // error

declare const indexer: {[string]: number, foo: string};
indexer.a!; // ok
indexer.foo!; // error
indexer['a']; // ok
indexer['foo']!; // error
