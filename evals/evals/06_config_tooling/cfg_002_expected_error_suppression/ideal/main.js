// @flow

export function clamp(value: number, min: number, max: number): number {
  return Math.min(Math.max(value, min), max);
}

// Contract checks — these document how `clamp` must behave.
clamp(5, 0, 10) as number;

// Intentionally invalid: callers must not pass a string. We want Flow to keep
// rejecting this so the contract stays enforced.
// $FlowExpectedError[incompatible-type]
clamp('5', 0, 10);
