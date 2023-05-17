function invalid_pattern_object({a: b}: {a: mixed}): boolean %checks {
  return typeof b === 'string'; // error b is in pattern
}

function invalid_pattern_array([b]: [mixed]): boolean %checks {
  return typeof b === 'string'; // error b is in pattern
}

function invalid_rest(...a: Array<mixed>): boolean %checks {
  return typeof a === 'string';  // error a is in rest param
}

declare function invalid_rest_decl(...a: Array<mixed>): boolean %checks(typeof a === 'string');  // error a is in rest param

function invalid_pattern_compound({a: [b]}: {a: [mixed]}, ...c: Array<mixed>): boolean %checks {
  return typeof b === 'string' && typeof c === 'number'; // error b, c in pattern
}

declare function valid_pattern_decl(x: mixed, y: {a: mixed}): boolean %checks(typeof y === 'string'); // okay

function valid_pattern({a}: {a: mixed}, x: mixed, ...r: Array<number>): boolean %checks {
  return typeof x === 'string'; // this is okay, pattern/rest does not affect predicate param
}

declare var x: mixed;
if (valid_pattern({a: 42}, x, 1, 2, 3)) {
  (x: string); // okay
  (x: number); // error string ~> number
}

function partially_valid({x}: {x: mixed}, y: mixed, ...r: Array<number>): boolean %checks {
  return (
    typeof x === 'string' && // error '[x' in pattern
    typeof y === 'string'    // this is okay, pattern/rest does not affect predicate param
  );
}

declare var a: mixed;
declare var b: mixed;

if (partially_valid({x: a}, b, 1, 2, 3)) {
  (a: string); // error mixed ~> string because 'a' is not refined
  (a: number); // error mixed ~> number because 'a' is not refined
  (b: string); // okay because 'b' is refined to string
  (b: number); // error string ~> number
}
