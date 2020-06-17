// @flow

import {is_string, is_number} from './pred-decls';

//
// Predicate identity
//
declare class C {}
declare function instance_of_C(node: mixed): boolean %checks(node instanceof C);

declare var a1: typeof instance_of_C;
(a1: typeof instance_of_C);

//
// Incompatible predicates
//
declare var a2: typeof is_string;
(a2: typeof is_string);

declare var a3: typeof is_string;
(a3: typeof is_number); // error incompatible predicates

function is_string_def(x: mixed): boolean %checks {
  return typeof x === 'string';
}
function is_number_def(x: mixed): boolean %checks {
  return typeof x === 'number';
}

declare var b1: typeof is_string_def;
(b1: typeof is_string_def);

declare var b2: typeof is_string_def;
(b2: typeof is_number_def); // error incompatible predicates

//
// Predicate function arity mismatch
//
function is_number_def_2a(x: mixed, y: mixed): boolean %checks {
  return typeof x === 'number' && typeof y === 'number';
}

declare var b3: typeof is_number_def_2a;
(b3: typeof is_number_def); // okay: at least 1 argument

declare var b4: typeof is_number_def;
(b4: typeof is_number_def_2a); // error: arrity mismatch


//
// Refinement weakening
///
function is_number_def_2b(x: mixed, y: mixed): boolean %checks {
  return typeof x === 'number';
}

declare var b5: typeof is_number_def_2a;
(b5: typeof is_number_def_2b); // okay: is_number_def_a refines at least `x`

declare var b6: typeof is_number_def_2b;
(b6: typeof is_number_def_2a); // error: is_number_def_2b does not refine at least `y`
