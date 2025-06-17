// `number`
{
  declare const x: number;

  match (x) { // OK
    1 => {}
    2 => {}
    _ => {}
  }

  match (x) { // ERROR: wildcard needed
    1 => {}
    2 => {}
  }
}

// Two inexhaustible
{
  declare const x: number | string;

  match (x) { // OK
    1 => {}
    's' => {}
    _ => {}
  }

  match (x) { // ERROR: wildcard needed
    1 => {}
    's' => {}
  }
}

// Three inexhaustible
{
  declare const x: number | string | bigint;

  match (x) { // OK
    _ => {}
  }

  match (x) { // ERROR: wildcard needed
  }
}

// Four inexhaustible
{
  declare const x: number | string | bigint | symbol;

  match (x) { // OK
    _ => {}
  }

  match (x) { // ERROR: wildcard needed
  }
}

// `string`
{
  declare const x: string;

  match (x) {
    'foo' => {} // OK
    'bar' => {} // OK
    _ => {}
  }
}

// StringPrefix
{
  declare const x: StringPrefix<'data-'>;

  match (x) {
    'data-foo' => {} // OK
    'data-bar' => {} // OK
    'xxx' => {} // ERROR
    _ => {}
  }
}

// StringSuffix
{
  declare const x: StringSuffix<'%'>;

  match (x) {
    '8%' => {} // OK
    '100%' => {} // OK
    'xxx' => {} // ERROR
    _ => {}
  }
}

// Abstract enum values
{
  declare const x: EnumValue<number>;

  enum N {
    A = 1,
  }
  enum S {
    A = 'a',
  }

  match (x) {
    N.A => {} // OK
    S.A => {} // ERROR
    _ => {}
  }
}

// Mixed and variants
{
  declare const x: mixed;

  match (x) {} // ERROR

  match (x) {
    0 => {} // OK
    true => {} // OK
    's' => {} // OK
    [0] => {} // OK
    {foo: 0, ...} => {} // OK
    _ => {}
  }

  if (x) {
    match (x) {
      0 => {} // ERROR
      1 => {} // OK
      false => {} // ERROR
      true => {} // OK
      '' => {} // ERROR
      'x' => {} // OK
      null => {} // ERROR
      _ => {}
    }
  }

  if (x != null) {
    match (x) {
      0 => {} // OK
      false => {} // OK
      '' => {} // OK
      null => {} // ERROR
      _ => {}
    }
  }

  if (x !== null) {
    match (x) {
      0 => {} // OK
      false => {} // OK
      '' => {} // OK
      undefined => {} // OK
      null => {} // ERROR
      _ => {}
    }
  }
}

// Any
{
  declare const x: any;

  match (x) {
    0 => {} // OK
    true => {} // OK
    's' => {} // OK
    [0] => {} // OK
    {foo: 0, ...} => {} // OK
    _ => {}
  }
}

// Deep structures with `mixed`
{
  declare const x: mixed;

  match (x) { // ERROR: missing `_`
    {foo: true, bar: ['bar', {value: _, ...}, ...], ...} => {} // OK
  }

  match (x) { // OK
    {foo: true, bar: ['bar', {value: _, ...}, ...], ...} => {} // OK
    _ => {}
  }
}
