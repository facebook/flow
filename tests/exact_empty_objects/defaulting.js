declare var o: ?{prop: number};

// ??
{
  const x = o ?? {};

  const a = x.prop;
  (a: number | void); // OK

  (a: true); // ERROR
  (a: number); // ERROR
  (a: void); // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  (prop: number | void); // OK
  (prop: number); // ERROR
}

// ||
{
  const x = o || {};

  const a = x.prop;
  (a: number | void); // OK

  (a: true); // ERROR
  (a: number); // ERROR
  (a: void); // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  (prop: number | void); // OK
  (prop: number); // ERROR
}

// ? :
{
  const x = o != null ? o : {};

  const a = x.prop;
  (a: number | void); // OK

  (a: true); // ERROR
  (a: number); // ERROR
  (a: void); // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  (prop: number | void); // OK
  (prop: number); // ERROR
}

// With non-exact hit
declare var n: ?{
  prop: number,
  ...
};

// ??
{
  const x = n ?? {};

  const a = x.prop;
  (a: number | void); // OK

  (a: true); // ERROR
  (a: number); // ERROR
  (a: void); // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  (prop: number | void); // OK
  (prop: number); // ERROR
}

// With instance hit
class C {
  prop: number;
}
declare var c: ?C;

// ??
{
  const x = c ?? {};

  const a = x.prop;
  (a: number | void); // OK

  (a: true); // ERROR
  (a: number); // ERROR
  (a: void); // ERROR
  (x.XXX); // ERROR

  // Destructuring
  const {prop} = x;
  (prop: number | void); // OK
  (prop: number); // ERROR
}

// any
declare var a: ?any;
{
  const {x} = a ?? {}; // OK
  const {y} = a || {}; // OK
  const {z} = a ? a : {}; // OK
}
