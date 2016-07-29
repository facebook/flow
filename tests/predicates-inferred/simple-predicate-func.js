// @flow

function $pred$is_string(y) {
  return typeof y === "string";
}

function $pred$is_bool(y) {
  return typeof y === "boolean";
}

function $pred$is_number(y) {
  return typeof y === "number";
}

// Feature check:
function foo(x: string | Array<string>): string {
  if ($pred$is_string(x)) {
    // The use of `is_string` as a conditional check
    // should guarantee the narrowing of the type of `x`
    // to string.
    return x;
  } else {
    // Accordingly the negation of the above check
    // guarantees that `x` here is an Array<string>
    return x.join();
  }
}

// Same as above but refining an offset
function bar(z: { f: string | Array<string>}): string {
  if ($pred$is_string(z.f)) {
    return z.f;
  } else {
    return z.f.join();
  }
}

function $pred$is_number_or_bool(y) {
  return $pred$is_number(y) || $pred$is_bool(y);
}

function baz(z: string | number): number {
  if ($pred$is_number_or_bool(z)) {
    return z;
  } else {
    return z.length;
  }
}


// Feature: multi params
function $pred$multi_param(w,x,y,z) {
  return typeof z === "string";
}

function foo(x: string | Array<string>): string {
  if ($pred$multi_param("1", "2", "3", x)) {
    return x;
  } else {
    return x.join();
  }
}
