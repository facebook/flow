// @flow

export function f({a: b}: {a: mixed}): boolean %checks {
  return typeof b === 'string';
}

declare var o: {a: mixed};
if (f(o)) {} // no error, predicate above is ignored

function g([b]: [number]): boolean %checks {
  return typeof b === 'string';
}

function h(...a: Array<mixed>): boolean %checks {
  return typeof a === 'string';
}

declare function i(...a: Array<mixed>): boolean %checks(typeof a === 'string');

function j({a: b}: {a: mixed}, ...c: Array<mixed>): boolean %checks {
  return typeof b === 'string' && typeof c === 'number';
}

declare function k(x: mixed, y: {a: mixed}): boolean %checks(typeof y ===
  'string');

declare function l(...x: Array<mixed>): boolean %checks(typeof x === 'string');

export function m(x: mixed, {a: b}: {a: mixed}): boolean %checks {
  return typeof b === 'string';
}
