// @flow

declare function invariant(...Array<mixed>): void;

function alwaysThrows() { throw '' }

function sometimesThrows() {
  if (true) invariant();
  return 3;
}

const a = invariant(false);
