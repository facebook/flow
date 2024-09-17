declare const o: {
  s: 's',
  n: 1,
  b: true,
  i: 1n,
};

{
  const {s = 'xxx'} = o; // ERROR
  s as 's'; // OK
}

{
  const {n = 999999} = o; // ERROR
  n as 1; // OK
}

{
  const {b = false} = o; // ERROR
  b as true; // OK
}

{
  const {i = 999999n} = o; // ERROR
  i as 1n; // OK
}
