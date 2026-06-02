/**
 * @format
 * @flow
 */

declare const any: any;
declare opaque type A;
declare opaque type B;
declare opaque type C;
declare opaque type D;

// Error: Group should list three errors in the order: `b`, `a`, `c`
any as {readonly b: boolean, readonly a: boolean, readonly c: boolean} as {
  readonly b: 2,
  readonly a: 1,
  readonly c: 3,
};

// Error: Group should list three errors in the order: `a`, `b`, `c`
any as {
  readonly b: boolean & string,
  readonly a: boolean & string,
  readonly c: boolean & string,
} as {readonly b: 2, readonly a: 1, readonly c: 3};

// Error: Group should list three errors in the order: `a`, `b`, `c`
any as {
  readonly b: boolean,
  readonly a: boolean & string,
  readonly c: boolean,
} as {
  readonly b: 2,
  readonly a: 1,
  readonly c: 3,
};

// Error: Group should list three errors in the order: `b`, `a`, `c`
any as {
  readonly b: boolean & string,
  readonly a: boolean,
  readonly c: boolean,
} as {
  readonly b: 2,
  readonly a: 1,
  readonly c: 3,
};

// Error: Group should list three errors in the order: `c`, `b`, `a`
any as {
  readonly b: boolean,
  readonly a: boolean,
  readonly c: boolean & string,
} as {
  readonly b: 2,
  readonly a: 1,
  readonly c: 3,
};

// Error: number ~> boolean
any as {readonly a: {readonly b: boolean}} as {readonly a: {readonly b: 42}};

// Error: number ~> boolean. Because of union error scoring we should only see
// one error.
any as {readonly a: boolean & {readonly b: boolean & {}}} as {
  readonly a: {readonly b: 42},
};

any as boolean as 42; // Error: number ~> boolean
any as {} & {} & {} & boolean as 42; // Error: number ~> boolean
any as {} & ({} & ({} & boolean)) as 42; // Error: number ~> boolean

any as number & string as true; // Error
any as number & string & {} as true; // Error: should not show the {} branch
any as {} & number & string as true; // Error: should not show the {} branch
any as number & {} & string as true; // Error: should not show the {} branch

any as {readonly a: number & string} as {readonly a: true}; // Error

any as {
  readonly a: number & string,
  readonly b: {} & number & string,
  readonly c: number & {} & string,
  readonly d: number & string & {},
} as {
  readonly a: true, // Error: should be grouped
  readonly b: true, // Error: should be grouped, should not show the {} branch
  readonly c: true, // Error: should be grouped, should not show the {} branch
  readonly d: true, // Error: should be grouped, should not show the {} branch
};

// Demonstrates use_op ~> union speculation ~> use_op
any as {
  readonly a: [number] & [string],
  readonly b: [{}] & [number] & [string],
  readonly c: [number] & [{}] & [string],
  readonly d: [number] & [string] & [{}],
} as {
  readonly a: [true], // Error: should be grouped
  readonly b: [true], // Error: should be grouped, should not show the [{}] branch
  readonly c: [true], // Error: should be grouped, should not show the [{}] branch
  readonly d: [true], // Error: should be grouped, should not show the [{}] branch
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
any as {readonly a: number & {}, readonly b: number & {}} as {
  readonly a: true,
  readonly b: true,
}; // Error
any as {readonly a: number & string & {}, readonly b: number & {}} as {
  readonly a: true,
  readonly b: true,
}; // Error

// Error: union inside union fun.
any as [[null, A] & [null, B]] & [[null, C] & [null, D]] as [[null, number]];

// Error: union inside union fun, but thanks to scoring we only show three.
any as [[null, A] & {}] & [[null, C] & [null, D]] as [[null, number]];

// Error: union inside union fun, but thanks to scoring we only show two.
any as [[null, A] & {}] & [[null, C] & {}] as [[null, number]];

// Error: union inside union fun, but thanks to scoring we only show one.
any as {} & [[null, C] & {}] as [[null, number]];

declare const f1: A | B;
f1(); // Error

declare const f2: A | (B | C);
f2(); // Error

declare const f3: A | (B | (C | D));
f3(); // Error
