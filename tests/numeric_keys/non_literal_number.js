// Access with `number`
{
  declare const o: {foo: 1};
  declare const n: number;
  o[n]; // ERROR
}

// Creation with `number`
{
  declare const n: number;
  const o = {
    [n]: true, // OK
  };
  o as {[number]: true}; // OK
  o[n] as true; // OK
}
{
  declare const n: number;
  declare const o1: {[number]: true};
  // Creation with spread and `number` key
  const o2 = {...o1, [n]: true};
  o2 as {[number]: true}; // OK
}
