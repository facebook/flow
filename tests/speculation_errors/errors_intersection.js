/**
 * @format
 * @flow
 */

declare var any: any;
declare opaque type A;
declare opaque type B;
declare opaque type C;
declare opaque type D;

// Error: Group should list three errors in the order: `b`, `a`, `c`
any as {+b: boolean, +a: boolean, +c: boolean} as {+b: 2, +a: 1, +c: 3};

// Error: Group should list three errors in the order: `a`, `b`, `c`
any as {
  +b: boolean & string,
  +a: boolean & string,
  +c: boolean & string,
} as {+b: 2, +a: 1, +c: 3};

// Error: Group should list three errors in the order: `a`, `b`, `c`
any as {+b: boolean, +a: boolean & string, +c: boolean} as {
  +b: 2,
  +a: 1,
  +c: 3,
};

// Error: Group should list three errors in the order: `b`, `a`, `c`
any as {+b: boolean & string, +a: boolean, +c: boolean} as {
  +b: 2,
  +a: 1,
  +c: 3,
};

// Error: Group should list three errors in the order: `c`, `b`, `a`
any as {+b: boolean, +a: boolean, +c: boolean & string} as {
  +b: 2,
  +a: 1,
  +c: 3,
};

// Error: number ~> boolean
any as {+a: {+b: boolean}} as {+a: {+b: 42}};

// Error: number ~> boolean. Because of union error scoring we should only see
// one error.
any as {+a: boolean & {+b: boolean & {}}} as {+a: {+b: 42}};

any as boolean as 42; // Error: number ~> boolean
any as {} & {} & {} & boolean as 42; // Error: number ~> boolean
any as {} & ({} & ({} & boolean)) as 42; // Error: number ~> boolean

any as number & string as true; // Error
any as number & string & {} as true; // Error: should not show the {} branch
any as {} & number & string as true; // Error: should not show the {} branch
any as number & {} & string as true; // Error: should not show the {} branch

any as {+a: number & string} as {+a: true}; // Error

any as {
  +a: number & string,
  +b: {} & number & string,
  +c: number & {} & string,
  +d: number & string & {},
} as {
  +a: true, // Error: should be grouped
  +b: true, // Error: should be grouped, should not show the {} branch
  +c: true, // Error: should be grouped, should not show the {} branch
  +d: true, // Error: should be grouped, should not show the {} branch
};

// Demonstrates use_op ~> union speculation ~> use_op
any as {
  +a: [number] & [string],
  +b: [{}] & [number] & [string],
  +c: [number] & [{}] & [string],
  +d: [number] & [string] & [{}],
} as {
  +a: [true], // Error: should be grouped
  +b: [true], // Error: should be grouped, should not show the [{}] branch
  +c: [true], // Error: should be grouped, should not show the [{}] branch
  +d: [true], // Error: should be grouped, should not show the [{}] branch
};

any as number & (string & false) as true; // Error: should be flattened
any as (string & false) & number as true; // Error: should be flattened
any as {} & number & (string & false) as true; // Error: should be flattened
any as number & (string & false) & {} as true; // Error: should be flattened
any as number & (string & {} & false) as true; // Error: should be flattened

any as number & number & number & string as true; // Error: should be flattened
any as number & number & (number & string) as true; // Error: should be flattened
any as number & (number & (number & string)) as true; // Error: should be flattened
any as (string & number) & number & number as true; // Error: should be flattened
any as ((string & number) & number) & number as true; // Error: should be flattened

any as number & {} as true; // Error
any as {+a: number & {}, +b: number & {}} as {+a: true, +b: true}; // Error
any as {+a: number & string & {}, +b: number & {}} as {+a: true, +b: true}; // Error

// Error: union inside union fun.
any as [[null, A] & [null, B]] & [[null, C] & [null, D]] as [[null, number]];

// Error: union inside union fun, but thanks to scoring we only show three.
any as [[null, A] & {}] & [[null, C] & [null, D]] as [[null, number]];

// Error: union inside union fun, but thanks to scoring we only show two.
any as [[null, A] & {}] & [[null, C] & {}] as [[null, number]];

// Error: union inside union fun, but thanks to scoring we only show one.
any as {} & [[null, C] & {}] as [[null, number]];

declare var f1: A | B;
f1(); // Error

declare var f2: A | (B | C);
f2(); // Error

declare var f3: A | (B | (C | D));
f3(); // Error
