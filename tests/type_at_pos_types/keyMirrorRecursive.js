// @flow

export type KeyMirrorRecursive<O> = {
  [K in keyof O]: O[K] extends {...} ? KeyMirrorRecursive<O[K]> : K,
};

declare function keyMirrorRecursive<O: {}>(
  obj: O,
  _: void,
): KeyMirrorRecursive<O>;

module.exports = keyMirrorRecursive;
