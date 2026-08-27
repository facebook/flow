class A {
  m(value?: string): number {
    return 0;
  }
}


{
  declare const a: A;
  const m: A['m'] = a.m; // OK - the extracted function retains `this: A`
  m as A['m']; // OK
  m(); // ERROR - the receiver is missing

  const y = a.m.bind(a);
  y('value') as number; // OK
  y(42); // ERROR - number is incompatible with string
  y('value') as string; // ERROR - number is incompatible with string
}
