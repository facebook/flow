declare const bad1: unknown;
declare const bad2: () => {};
declare const good1: number;
declare const good2: {foo: string};

// error: undefined is not compatible with string
JSON.stringify(bad1) as string;

// TODO should error, but currently does not. We allow functions to be coerced to objects
JSON.stringify(bad2) as string;

JSON.stringify(good1) as string; // ok
JSON.stringify(good2) as string; // ok
