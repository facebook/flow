// Global `NaN` as a pattern. Has type `number`.
{
  declare const x: number;

  match (x) { // ERROR: wildcard still needed, `NaN` doesn't contribute to exhaustiveness
    1 => {}
    NaN => {} // OK: not invalid, not unused
  }

  match (x) { // OK
    1 => {}
    NaN => {} // OK: not invalid, not unused
    _ => {}
  }
}

// `NaN` with a non-number input cannot match.
{
  declare const x: string;

  match (x) {
    'foo' => {}
    NaN => {} // ERROR: unused, `NaN` can never match a string
    _ => {}
  }
}

// `NaN` in a union including `number`.
{
  declare const x: number | string;

  match (x) { // OK
    's' => {}
    NaN => {} // OK
    _ => {}
  }
}

// Duplicate `NaN`.
{
  declare const x: number;

  match (x) { // OK
    NaN => {}
    NaN => {} // ERROR: unused, already seen
    _ => {}
  }
}

// `NaN` after a wildcard.
{
  declare const x: number;

  match (x) {
    _ => {}
    NaN => {} // ERROR: unused, already matched by wildcard
  }
}

// Locally-defined `NaN` should not be special-cased.
{
  declare const x: number;
  const NaN = 1;

  match (x) { // ERROR: wildcard needed
    NaN => {} // OK: treated as the literal `1`
  }

  match (x) { // OK
    NaN => {} // treated as the literal `1`
    1 => {} // ERROR: unused, `NaN` is `1`
    _ => {}
  }
}
