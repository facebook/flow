class A {
  m(value?: string): number {
    return 0;
  }
}


{
  declare const a: A;
  const m: A['m'] = a.m; // ERROR - the indexed access has `this: mixed`
  m as A['m']; // OK
  m(); // OK - the indexed access has `this: mixed`

  const y = a.m.bind(a);
  y('value') as number; // OK
  y(42); // ERROR - number is incompatible with string
  y('value') as string; // ERROR - number is incompatible with string
}
