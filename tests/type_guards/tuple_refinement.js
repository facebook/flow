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
