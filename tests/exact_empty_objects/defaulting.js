declare const o: ?{prop: number};

// ??
{
  const x = o ?? {};

  const a = x.prop;
  a as number | void; // OK

  a as true; // ERROR
  a as number; // ERROR
  a as void; // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  prop as number | void; // OK
  prop as number; // ERROR
}

// ||
{
  const x = o || {};

  const a = x.prop;
  a as number | void; // OK

  a as true; // ERROR
  a as number; // ERROR
  a as void; // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  prop as number | void; // OK
  prop as number; // ERROR
}

// ? :
{
  const x = o != null ? o : {};

  const a = x.prop;
  a as number | void; // OK

  a as true; // ERROR
  a as number; // ERROR
  a as void; // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  prop as number | void; // OK
  prop as number; // ERROR
}

// With non-exact hit
declare const n: ?{
  prop: number,
  ...
};

// ??
{
  const x = n ?? {};

  const a = x.prop;
  a as number | void; // OK

  a as true; // ERROR
  a as number; // ERROR
  a as void; // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  prop as number | void; // OK
  prop as number; // ERROR
}

// With instance hit
class C {
  prop: number;
}
declare const c: ?C;

// ??
{
  const x = c ?? {};

  const a = x.prop;
  a as number | void; // OK

  a as true; // ERROR
  a as number; // ERROR
  a as void; // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  prop as number | void; // OK
  prop as number; // ERROR
}

// any
declare const a: ?any;
{
  const {x} = a ?? {}; // OK
  const {y} = a || {}; // OK
  const {z} = a ? a : {}; // OK
}

// Computed props
{
  const x = o ?? {};

  const a = x['prop'];
  a as number | void; // OK
}
