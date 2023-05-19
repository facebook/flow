function invalid_pattern_object({a: b}: {a: mixed}): b is string {
  return typeof b === 'string'; // error b is in pattern
}

function invalid_pattern_array([b]: [mixed]): b is string {
  return typeof b === 'string'; // error b is in pattern
}

function invalid_rest(...a: Array<mixed>): a is string {
  return typeof a === 'string';  // error a is in rest param
}

declare function invalid_rest_decl(...a: Array<mixed>): a is string;  // error a is in rest param

function invalid_pattern_compound({a: [b]}: {a: [mixed]}): b is string {
  return typeof b === 'string'; // error b
}

declare function valid_pattern_decl(x: mixed, y: {a: mixed}): x is string; // okay

function valid_pattern({a}: {a: mixed}, x: mixed, ...r: Array<number>): x is string {
  return typeof x === 'string'; // this is okay, pattern/rest does not affect predicate param
}

declare var x: mixed;
if (valid_pattern({a: 42}, x, 1, 2, 3)) {
  (x: string); // TODO okay
  (x: number); // TODO error string ~> number
}

type F = (x: mixed, ...p: Array<mixed>) => p is number; // error p is in rest param
