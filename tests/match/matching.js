// Primitive value union
{
  declare const x: 1 | -2 | 3n | 's' | false | null;

  const e1 = match (x) {
    1: 0,
    -2: 0,
    3n: 0,
    's': 0,
    false: 0,
    null: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    1: 0,
    false: 0,
    3n: 0,
    null: 0,
    const d: d as empty, // ERROR: not all members checked
  };
}

// Identifier patterns
{
  declare const x: 1 | 2;

  declare const one: 1;
  declare const two: 2;

  const e1 = match (x) {
    one: 0,
    two: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    one: 0,
    const d: d as empty, // ERROR: `2` not checked
  };
}

// `undefined`
{
  declare const x: 1 | void;

  const e1 = match (x) {
    1: 0,
    undefined: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    1: 0,
    const d: d as empty, // ERROR: `undefined` not checked
  };
}

// Maybe types
{
  declare const x: ?1;

  const e1 = match (x) {
    1: 0,
    undefined: 0,
    null: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    1: 0,
    const d: d as empty, // ERROR: `null` and `undefined` not checked
  };
}

// Member patterns
{
  declare const x: 1 | 2;

  declare const o: {
    one: 1,
    two: 2,
  };

  const e1 = match (x) {
    o.one: 0,
    o.two: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    o.one: 0,
    const d: d as empty, // ERROR: `2` not checked
  };
}

// `as` pattern refines using its pattern
{
  declare const x: 1 | 2;

  const e1 = match (x) {
    1 as a: a as 1, // OK
    2 as a: a as 2, // OK
    const d: d as empty, // OK: all members checked
  };
}


// Top level binding and wildcard
{
  declare const x: 1 | 2;

  const e1 = match (x) {
    1: 0,
    const a: a as 2, // OK
    const d: d as empty, // OK: above binding catches all
  };

  const e2 = match (x) {
    1: 0,
    _: 0, // OK
    const d: d as empty, // OK: above wildcard catches all
  };
}

// Non-ident/member argument still works
{
  declare const f: () => 1 | 2;

  const e1 = match (f()) {
    1: 0,
    2: 0,
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (f()) {
    1: 0,
    const d: d as empty, // ERROR: `2` not checked
  };
}

// Or pattern
{
  declare const x: 1 | 2 | 3;

  const e1 = match (x) {
    1 | 2 | 3: true,
    const d: d as empty, // OK
  };

  const e2 = match (x) {
    1 | 2: true,
    const d: d as empty, // ERROR: `3` not checked
  };
}

// Patterns with guard could match or not match
{
  declare const x: 1 | 2;

  declare function f(): boolean;

  const e1 = match (x) {
    1: 0,
    2 if f(): 0,
    const d: d as empty, // ERROR: `2` not checked
  };

  const e2 = match (x) {
    1: 0,
    2 if f(): 0,
    2: 0,
    const d: d as empty, // OK
  };
}

// Property exists
{
  declare const x: {foo: void, a: 0} | {bar: void, a: 1};

  const e1 = match (x) {
    {foo: _, const a}: a as 0, // OK
    {bar: _, const a}: a as 1, // OK
    const d: d as empty, // OK: all members checked
  };
}

// Disjoint object union
{
  declare const x: {type: 'foo', val: number}
                 | {type: 'bar', val: string}
                 | {type: 'baz', val: boolean};

  const e1 = match (x) {
    {type: 'foo', val: const a}: a as number, // OK
    {type: 'bar', val: const a}: a as string, // OK
    {type: 'baz', val: const a}: a as boolean, // OK
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    {type: 'foo', val: const a}: a as number, // OK
    {type: 'bar', val: const a}: a as string, // OK
    const d: d as empty, // ERROR: `type: 'baz'` not checked
  };

  // Using idents as pattern
  declare const foo: 'foo';
  declare const bar: 'bar';
  declare const baz: 'baz';
  const e3 = match (x) {
    {type: foo, val: const a}: a as number, // OK
    {type: bar, val: const a}: a as string, // OK
    {type: baz, val: const a}: a as boolean, // OK
    const d: d as empty, // OK: all members checked
  };

  // Using members as pattern
  declare const o: {
    foo: 'foo',
    bar: 'bar',
    baz: 'baz',
  };
  const e4 = match (x) {
    {type: o.foo, val: const a}: a as number, // OK
    {type: o.bar, val: const a}: a as string, // OK
    {type: o.baz, val: const a}: a as boolean, // OK
    const d: d as empty, // OK: all members checked
  };
}

// Disjoint object union with multiple pivot props
{
  declare const x: {type: 'foo', val: number}
                 | {type: 'bar', n: 1, val: string}
                 | {type: 'bar', n: 2, val: boolean};

  const e1 = match (x) {
    {type: 'foo', val: const a}: a as number, // OK
    {type: 'bar', val: const a}: a as string | boolean, // OK
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    {type: 'foo', val: const a}: a as number, // OK
    {type: 'bar', n: 1, val: const a}: a as string, // OK
    {type: 'bar', n: 2, val: const a}: a as boolean, // OK
    const d: d as empty, // OK: all members checked
  };

  const e3 = match (x) {
    {type: 'foo', val: const a}: a as number, // OK
    {type: 'bar', n: 1, val: const a}: a as string, // OK
    const d: d as empty, // ERROR: `type: 'bar', n: 2` not checked
  };
}

// Combo union of object with sentinel property and primitive value
{
  declare const x: null | {type: 'bar', val: number};

  const e1 = match (x) {
    {type: 'bar', val: const a}: a as number, // OK
    null: 0,
    const d: d as empty, // OK: all members checked
  };
}

// Or pattern: objects
{
  declare const x: {type: 'foo', val: number}
                 | {type: 'bar', val: string}
                 | {type: 'baz', val: boolean};

  const e1 = match (x) {
    {type: 'foo'} | {type: 'bar'} | {type: 'baz'}: 0,
    const d: d as empty, // OK
  };

  const e2 = match (x) {
    {type: 'foo'} | {type: 'baz'}: 0,
    const d: d as empty, // ERROR: `type: 'bar'` not checked
  };
}

// Disjoint tuple union
{
  declare const x: ['foo', number]
                 | ['bar', string]
                 | ['baz', boolean];

  const e1 = match (x) {
    ['foo', const a]: a as number, // OK
    ['bar', const a]: a as string, // OK
    ['baz', const a]: a as boolean, // OK
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    ['foo', const a]: a as number, // OK
    ['bar', const a]: a as string, // OK
    const d: d as empty, // ERROR: `'baz'` element not checked
  };

  // Using idents as pattern
  declare const foo: 'foo';
  declare const bar: 'bar';
  declare const baz: 'baz';
  const e3 = match (x) {
    [foo, const a]: a as number, // OK
    [bar, const a]: a as string, // OK
    [baz, const a]: a as boolean, // OK
    const d: d as empty, // OK: all members checked
  };
}

// Combo union of tuples with sentinel property and primitive value
{
  declare const x: null | ['bar', number] | ['foo', string];

  const e1 = match (x) {
    ['bar', const a]: a as number, // OK
    ['foo', const a]: a as string, // OK
    null: 0,
    const d: d as empty, // OK: all members checked
  };
}

// Tuple length refinements
{
  declare const x: [number]
                 | [string, string]
                 | [boolean, boolean, boolean];

  const e1 = match (x) {
    [const a]: a as number, // OK
    [const a, _]: a as string, // OK
    [const a, _, _]: a as empty, // ERROR: `boolean` is not `empty`
    const d: d as empty, // OK: all members checked
  };

  const e2 = match (x) {
    [...]: 0, // OK: matches all
    const d: d as empty, // OK: all members checked
  }

  const e3 = match (x) {
    [const a, _, ...]: a as string | boolean, // OK
    [const a, ...]: a as number, // OK
    const d: d as empty, // OK: all members checked
  }
}
{
  declare const x: [number] | Array<string>;

  const e1 = match (x) {
    []: 0, // OK
    [const a]: a as string, // ERROR: `number` is not `string`
    [const a, _]: a as string, // OK
    const d: d as Array<string>, // OK: tuple checked, but array could have other lengths
  };

  const e2 = match (x) {
    [...]: 0, // OK: matches all
    const d: d as empty, // OK: all members checked
  }
}

// Optional tuple elements
{
  declare const x: [a: 0, b?: 1, c?: 2];

  const e1 = match (x) {
    [_, ...]: 0,
    const d: d as empty, // OK: all elements matched
  };

  const e2 = match (x) {
    [_, _, ...]: 0,
    const d: d as empty, // ERROR: does not match all possibilities
  };

  const e3 = match (x) {
    [_]: 0,
    const d: d as empty, // ERROR: does not match all possibilities
  };

  const e4 = match (x) {
    [_, _, _]: 0,
    const d: d as empty, // ERROR: does not match all possibilities
  };
}

// Inexact tuple types
{
  declare const x: [a: 0, ...];

  const e1 = match (x) {
    [_, ...]: 0,
    const d: d as empty, // OK: all elements matched
  };

  const e2 = match (x) {
    [_]: 0,
    const d: d as empty, // ERROR: does not match all elements
  };
}
