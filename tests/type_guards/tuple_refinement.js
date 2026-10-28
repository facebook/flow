// Simple
type A = ['A', number];
type B = ['B', string];

{
  declare const x: A | B;

  declare function sentinel_A(x: unknown): x is ['A', ...];
  if (sentinel_A(x)) {
      x as A; // OK: A ~> A
      x as B; // ERROR: A ~> B
  }

  declare function invalid_sentinel_val(x: unknown): x is ['C', ...];
  if (invalid_sentinel_val(x)) {
      x as A; // OK: empty ~> A
      x as B; // OK: empty ~> B
  }
}

// Readonly
type ROA = Readonly<['A', number]>;
type ROB = Readonly<['B', string]>;

{
  declare const x: ROA | ROB;

  declare function sentinel_A(x: unknown): x is Readonly<['A', ...]>;
  if (sentinel_A(x)) {
      x as ROA; // OK: A ~> A
      x as ROB; // ERROR: A ~> B
  }

  declare function invalid_sentinel_val(x: unknown): x is Readonly<['C', ...]>;
  if (invalid_sentinel_val(x)) {
      x as ROA; // OK: empty ~> A
      x as ROB; // OK: empty ~> B
  }
}

// Multi tag
type MTA = Readonly<['A', 1, number]>;
type MTB = Readonly<['B', 1, string]>;

{
  declare const x: MTA | MTB;

  declare function sentinel_A_1(x: unknown): x is Readonly<['A', 1, ...]>;
  if (sentinel_A_1(x)) {
      x as MTA; // OK: A ~> A
      x as MTB; // ERROR: A ~> B
  }
}

// Length
{
  declare const x: [number] | [number, string];

  declare function isPair(x: unknown): x is [unknown, unknown];
  if (isPair(x)) {
      x as [number, string]; // OK: [unknown, unknown] & [number, string]
      x as [number]; // ERROR: [unknown, unknown] & [number, string] ~> [number]
  }

  declare function isNumberPair(x: unknown): x is [number, string];
  if (isNumberPair(x)) {
      x as [number, string]; // OK
  }
}

// Elements
{
  declare const x: [number, string] | [string, string];

  declare function isStartNumber(x: unknown): x is Readonly<[number, ...]>;
  if (isStartNumber(x)) {
      x as [number, string]; // OK
  }
}

// Arrays
{
  declare const x: Array<string> | Array<number>;

  declare function isStrings(x: unknown): x is Array<string>;
  if (isStrings(x)) {
      x as Array<string>; // OK
  }

  declare const y: ReadonlyArray<string> | ReadonlyArray<number>;
  declare function isROStrings(x: unknown): x is ReadonlyArray<string>;
  if (isROStrings(y)) {
      y as ReadonlyArray<string>; // OK
  }
}

// Optional and rest elements
{
  declare function isTriple(x: unknown): x is [unknown, unknown, unknown];

  declare const x: [a: number, b?: string];
  if (isTriple(x)) {
      x as empty; // OK: at most two elements
  }

  declare const y: [number, ...Array<string>];
  if (isTriple(y)) {
      y as empty; // ERROR: any number of elements
  }

  declare function isShort(x: unknown): x is [a: unknown, b?: unknown];
  declare const z: [number, number, number];
  if (isShort(z)) {
      z as empty; // OK: at least three elements
  }

  declare function isNumberString(x: unknown): x is [a: number, b?: string];
  declare const w: [a: number, b?: number];
  if (isNumberString(w)) {
      w as empty; // ERROR: [1] is both, optional elements are not compared
  }
}

// Write-only elements
{
  declare function isNumber1(x: unknown): x is [number];
  declare const x: [writeonly a: string];
  if (isNumber1(x)) {
      x as empty; // ERROR: a write-only element can hold any value
  }
}

// Arrays of `empty`
{
  declare function isNumbers(x: unknown): x is Array<number>;
  declare const x: Array<empty>;
  if (isNumbers(x)) {
      x as empty; // ERROR: [] is both
  }
}
