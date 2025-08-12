type Exact = Record<'foo' | 'bar', number>;

declare const exact: Exact;
(exact: {foo: number, bar: number}); // OK!
(exact: {foo: number}); // ERROR, missing bar

type Indexed = Record<'foo' | string, number>;
declare const indexed: Indexed;
(indexed: {foo: number, [string]: number}); // OK
(indexed: {foo: number}); // ERROR, missing indexer
