// @flow

declare function invariant(...Array<mixed>): void;

function alwaysThrows() { throw '' }

function sometimesThrows() {
  if (true) invariant();
  return 3;
}

const a = invariant(false);

const unreachableFunctionDef1 = function named() {} // Only expect unreachable error here
const unreachableFunctionDef2 = function () {} // Only expect unreachable error here
