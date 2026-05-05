function invalid_pattern_object({a: b}: {a: unknown}): b is string {
  return typeof b === 'string'; // error b is in pattern
}

function invalid_pattern_array([b]: [unknown]): b is string {
  return typeof b === 'string'; // error b is in pattern
}

function invalid_rest(...a: Array<unknown>): a is string {
  return typeof a === 'string';  // error a is in rest param
}

declare function invalid_rest_decl(...a: Array<unknown>): a is string;  // error a is in rest param

function invalid_pattern_compound({a: [b]}: {a: [unknown]}): b is string {
  return typeof b === 'string'; // error b
}

declare function valid_pattern_decl(x: unknown, y: {a: unknown}): x is string; // okay

function valid_pattern({a}: {a: unknown}, x: unknown, ...r: Array<number>): x is string {
  return typeof x === 'string'; // this is okay, pattern/rest does not affect predicate param
}

declare var x: unknown;
if (valid_pattern({a: 42}, x, 1, 2, 3)) {
  x as string; // TODO okay
  x as number; // TODO error string ~> number
}

type F = (x: unknown, ...p: Array<unknown>) => p is number; // error p is in rest param
