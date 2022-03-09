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
