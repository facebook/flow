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
    [n]: 1, // ERROR
  };
}
