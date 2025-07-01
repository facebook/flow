// Tests just about the error message formatting

// Top level ordering
{
  declare const x: {...} | [...];

  match (x) {} // ERROR
}
{
  declare const x: {...} | {foo: 0} | {foo: 0, bar: 0};

  match (x) {} // ERROR
}
{
  declare const x: [...] | [0, 0, ...] | [0, 0] | [0, ...];

  match (x) {} // ERROR
}

// Nested ordering
{
  declare const x: [{...} | {foo: 0} | {foo: 0, bar: 0}] | [0];

  match (x) { // ERROR
    [0] => {}
  }
}
{
  declare const x: [[...] | [0, 0, ...] | [0, 0] | [0, ...]] | [0];

  match (x) { // ERROR
    [0] => {}
  }
}
{
  declare const x: [{...} | [...]] | [0]

  match (x) { // ERROR
    [0] => {}
  }
}

// Arrays: empty inexact tuple pattern
{
  declare const x: {a: Array<boolean>};

  match (x) { // ERROR
    {a: [true]} => {}
  }
}

// Arrays: no duplicate reasons
{
  declare const x: Array<[1, 2]>;
  match (x) { // ERROR
    [[1, _], [_ ,2]] => {} // OK
    [[_, ...]] => {} // OK
  }
}

// Arrays: no duplicate patterns
{
  declare const x: Array<boolean> | Array<number> | [...];

  match (x) {} // ERROR: single suggested pattern
}

// Objects: many wildcard props
{
  declare const x: {a: number, b: string, c: boolean, d: symbol, e: bigint, f: 5};

  match (x) {} // ERROR

  match (x) {
    {a: 0, b: "", c: true, ...} => {}
  }
}

// Objects: Error for disjoint object union
{
  declare const x:
    | {type: 'ok', value: number}
    | {type: 'error', value: Error};

  match (x) {} // ERROR: `type` prop values are printed
}


// Objects: Deep object left over
{
  declare const x: {foo: {bar: {baz: {zap: {bort: boolean}}}}};

  match (x) {} // ERROR: only prints `foo: _`
}

// Objects: Sentinel prop is last, but printed first
{
  declare const x:
    | {a: 1 | 2, b: 1 | 2, xxx: 'foo'}
    | {a: 2 | 3, b: 2 | 3, xxx: 'bar'}

  match (x) { // ERROR: missing `{xxx: 'bar', a: _, b: _}`
    {a: 1 | 2, b: 1 | 2, xxx: 'foo'} => {}
  }

  match (x) { // ERROR: `xxx` prop is printed first in pattern
    {a: 1, b: 1, xxx: 'foo'} => {}
  }
}
