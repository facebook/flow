type A = {
  1: string,
};
{
  declare const o: A;

  o[1] as string; // OK
  o['1'] as string; // OK

  o[1] as empty; // ERROR
  o['1'] as empty; // ERROR
}

// With variance
type B = {
  -1: string, // ERROR
  +2: boolean, // ERROR
};
type C = {
  -'1': string, // OK
  +'2': boolean, // OK
};
{
  declare const o: C;
  o[2] as boolean; // OK
}

// Invalid numbers
type D = {
  1.1: true, // ERROR
  9007199254740992: false, // ERROR
};
