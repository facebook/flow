declare function invariant(condition?: unknown, message?: string): asserts condition;

function alwaysThrows() { throw '' }

function sometimesThrows() {
  if (true) invariant();
  return 3;
}

invariant(false);

const unreachableFunctionDef1 = function named() {} // Only expect unreachable error here
const unreachableFunctionDef2 = function () {} // Only expect unreachable error here
