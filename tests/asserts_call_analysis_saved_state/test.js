/* @flow */

export function assertNumber(value: unknown): asserts value is number {
  if (typeof value !== 'number') {
    throw 0;
  }
}

const value: unknown = 42;
assertNumber(value);
