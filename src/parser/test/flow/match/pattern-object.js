const e = match (x) {
  {foo: 1, bar: 2} => 1,
  {'foo': 1} => 1,
  {111: true} => 1,
  {foo: const y} => y,
  {const x, let y, var z} => y,
  {const x, ...const y} => y,
  {const x, ...let y} => y,
  {const x, ...var z} => y,
  {const x, ...} => 1,
  {x} => 1,
  {x, foo: 1} => 1,
  {1n: 1} => 1,
};
