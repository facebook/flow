// @flow

// Test: ++ and -- should NOT appear as tokens in type position.
// In type mode, + and - are variance annotations, not increment/decrement.

// Covariant and contravariant properties
type WithVariance = {
  readonly readonlyProp: string,
  writeonly writeonlyProp: number,
 ...};

// Multiple variance annotations on interface (each + is separate)
interface I {
  readonly x: string;
  writeonly y: number;
}

var obj: WithVariance = {readonlyProp: 'hello', writeonlyProp: 42};
