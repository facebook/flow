// @flow

// Sanity check: shouldn't be allowed to
// - declare a predicate return type ($StrP)
// AND
// - use `$pred$` as the name

function $pred$check(y): $StrP<0> {
  return typeof y === "number";
}

declare var y: number | boolean;

if ($pred$check(y)) {
  (y: number);
}

// Sanity: disallowed body
function $pred$indirect_is_number(y) {
  var y = 1;
  return typeof y === "number";
}

function bak(z: string | number): number {
  if ($pred$indirect_is_number(z)) {
    return z;
  } else {
    return z.length;
  }
}
