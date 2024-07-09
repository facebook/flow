// Simple
type A = ['A', number];
type B = ['B', string];

{
  declare const x: A | B;

  declare function sentinel_A(x: mixed): x is ['A', ...];
  if (sentinel_A(x)) {
      x as A; // OK: A ~> A
      x as B; // ERROR: A ~> B
  }

  declare function invalid_sentinel_val(x: mixed): x is ['C', ...];
  if (invalid_sentinel_val(x)) {
      x as A; // OK: empty ~> A
      x as B; // OK: empty ~> B
  }
}

// Readonly
type ROA = $ReadOnly<['A', number]>;
type ROB = $ReadOnly<['B', string]>;

{
  declare const x: ROA | ROB;

  declare function sentinel_A(x: mixed): x is $ReadOnly<['A', ...]>;
  if (sentinel_A(x)) {
      x as ROA; // OK: A ~> A
      x as ROB; // ERROR: A ~> B
  }

  declare function invalid_sentinel_val(x: mixed): x is $ReadOnly<['C', ...]>;
  if (invalid_sentinel_val(x)) {
      x as ROA; // OK: empty ~> A
      x as ROB; // OK: empty ~> B
  }
}

// Multi tag
type MTA = $ReadOnly<['A', 1, number]>;
type MTB = $ReadOnly<['B', 1, string]>;

{
  declare const x: MTA | MTB;

  declare function sentinel_A_1(x: mixed): x is $ReadOnly<['A', 1, ...]>;
  if (sentinel_A_1(x)) {
      x as MTA; // OK: A ~> A
      x as MTB; // ERROR: A ~> B
  }
}
