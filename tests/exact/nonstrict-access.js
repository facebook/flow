type T = {|
  all: string,
  some: string,
|} | {|
  all: string,
  some: string,
|} | {|
  all: string,
|};


{
  declare const x: T;

  // Prop access
  x.all as string; // OK
  x['all'] as string; // OK
  x.some as string; // ERROR
  x['some'] as string; // ERROR
  x.some as string | void; // OK
  x['some'] as string | void; // OK
  (x.NONE); // ERROR
  (x['NONE']); // ERROR

  // Destructuring
  const {all, some} = x;
  all as string; // OK
  some as string; // ERROR
  some as string | void; // OK

  const {NONE} = x; // ERROR
}

// Object literals are exact
{
  declare const cond: boolean;
  const toSpread = cond ? {all: "foo"} : {all: "foo", some: "bar"};
  const obj = {
    ...toSpread,
    baz: 2,
  };

  // Prop access
  obj.all as string; // OK
  obj.baz as number; // OK
  obj.some as string | void; // OK
  obj.some as string; // ERROR

  // Destructuring
  const {all, some, baz} = obj;
  all as string; // OK
  baz as number; // OK
  some as string | void; // OK
  some as string; // ERROR
}

// Intersection
{
  // It should not be possible to create such an object (intersection of
  // different exact objects), but unfortunately we have many instances
  // of this (e.g. React props) that exist due to other unsoundness issues.
  declare const o: {|a: number|} & {|b: string|};

  // Prop access
  o.a as number; // OK
  o.b as string; // OK

  const {a, b} = o;
  a as number; // OK
  b as string; // OK
}

// Computed with union
{
  declare const Foo:{
    foo: 1,
    bar: 2,
  };
  type K = 'foo' | 'bar' | 'xxx';
  declare const k: K;

  Foo[k]; // ERROR: prop-missing
}
