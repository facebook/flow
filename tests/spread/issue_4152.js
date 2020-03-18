// @flow

/**
 * This regression test exercises a bug reported
 * in https://github.com/facebook/flow/issues/4152
 */

export function wrapAndFlatten<T>(arr: Array<T>): mixed {
  const mapped = arr.map((n: T) => {
    return [n];
  });
  return mapped.reduce((x, y) => [...x, ...y]);
};
