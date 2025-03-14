// Top level binding with `const`
{
  declare const x: number;

  const out = match (x) {
    const a: a,
  };
  out as number; // OK
  out as empty; // ERROR
}

// Binding doesn't leak to outer scope
{
  declare const x: number;

  const out = match (x) {
    const a: a,
  };

  a; // ERROR: cannot resolve name
}

// Array pattern
{
  declare const x: [number];

  const out = match (x) {
    [const a]: a,
  };
  out as number; // OK
  out as empty; // ERROR
}

// Object pattern
{
  declare const x: {foo: number};

  const out = match (x) {
    {foo: const a}: a,
  };

  out as number; // OK
  out as empty; // ERROR
}

// Object shorthand pattern
{
  declare const x: {foo: number};

  const out = match (x) {
    {const foo}: foo,
  };

  out as number; // OK
  out as empty; // ERROR
}

// Nested patterns
{
  declare const x: {foo: [{bar: number}]};

  const out = match (x) {
    {foo: [{bar: const a}]}: a,
    {foo: const a}: a[0].bar,
  };

  out as number; // OK
  out as empty; // ERROR
}

// Guards
{
  declare const x: {foo: number};
  declare const f: number => boolean;

  const out = match (x) {
    {foo: const n} if n === 0: n, // OK
    {foo: const n} if f(n): n, // OK
    {foo: const n} if n as empty: n, // ERROR
    _: 0,
  };
}

// As pattern
{
  declare const x: {foo: [number]};

  const out = match (x) {
    {foo: [1] as n}: n as [number], // OK
    {foo: [1 as n]}: n as number, // OK
    _: 0,
  };
}
{
  declare const x: {foo: [number]};

  const out = match (x) {
    {foo: [1] as const n}: n as [number], // OK
    {foo: [1 as const n]}: n as number, // OK
    _: 0,
  };
}
{
  declare const x: {foo: {bar: number}};

  const out = match (x) {
    {foo: {bar: _} as const a}: 0, // OK
    {foo: {bar: 1}}: 0, // OK
    {foo: {bar: const a}}: 0, // OK
    {foo: {bar: 1 as const a}}: 0, // OK
    _: 0,
  };
}

// Array rest
{
  declare const x: [1, 2, 3];

  const out = match (x) {
    [...const xs]: xs as [1, 2, 3], // OK
    [1, ...const xs]: xs as [2, 3], // OK
    [1, 2, 3, ...const xs]: xs as [], // OK
    _: 0,
  };
}

// Object rest
{
  declare const x: {foo: 1, bar: 2, baz: 3};

  const out = match (x) {
    {...const xs}: xs as {foo: 1, bar: 2, baz: 3}, // OK
    {bar: _, ...const xs}: xs as {foo: 1, baz: 3}, // OK
    {foo: _, bar: _, baz: _, ...const xs}: xs as {}, // OK
    _: 0,
  };
}

// Identifier, member, literal
{
  declare const A: 1;
  declare const O: {B: 2};
  declare const x: number;

  const out = match (x) {
    0: 0,
    -1: 0,
    +1: 0,
    A: 0,
    O.B: 0,
    _: 0,
  };
}

// Or pattern
{
  declare const x: number;

  const out = match (x) {
    1 | 2 | 3: 0,
    _: 0,
  };
}

// Array pattern applied to array literal
{
  const x = [1, 'foo'];

  const out = match (x) {
    [_, const b]: b as string, // OK
  };
}

// BigInt member pattern
{
  declare const x: string;
  declare const O: {[bigint]: 'foo'};

  const out = match (x) {
    O[1n]: 0, // OK
    _: 0,
  };
}
