{
  declare const x: [string];
  const [a] = x;
  a as string; // OK
  a as empty; // ERROR
}

// Non-arrays/tuples
{
  declare const o: {foo: 1};
  const [x] = o; // ERROR
}

{
  declare const f: number => boolean;
  const [x] = f; // ERROR
}
