// @flow

type Literal = 'foo';
declare var x: Literal;
declare var o: {p: Literal}

switch (x) {
  case 'foo':
  case 'bar': // Error
  case 1: // Error
    break;
}

switch (o.p) {
  case 'foo':
  case 'bar': // Error
  case 1: // Error
    break;
}

if (x === 'foo') {}
if (x === 'bar') {} // Error
if (x === 1) {} // Error

if (o.p === 'foo') {}
if (o.p === 'bar') {} // Error
if (o.p === 1) {} // Error

// Even if the predicate is not used in refinements, we still error on literal subtyping
if (true && (false || x === 'bar')) {} // Error
if (true && (false || o.p === 'bar')) {} // Error
if (true && (false || x === 1)) {} // Error
if (true && (false || o.p === 1)) {} // Error

declare var nullableX: ?Literal;
declare var nullableO: ?{p: Literal}

function switch_optional_chaining_test() {} {
  switch (nullableX) {
    case 'foo':
    case 'bar': // Error
    case 1: // Error
      break;
  }

  switch (nullableO?.p) {
    case 'foo':
    case 'bar': // Error
    case 1: // Error
      break;
  }
}

function if_optional_chaining_test() {
  if (nullableX === 'foo') {}
  if (nullableX === 'bar') {} // Error
  if (nullableX === 1) {} // Error

  if (nullableO?.p === 'foo') {}
  if (nullableO?.p === 'bar') {} // Error
  if (nullableO?.p === 1) {} // Error
}

function unused_refinement_optional_chaining_test() {
  // Even if the predicate is not used in refinements, we still error on literal subtyping
  if (true && (false || nullableX === 'bar')) {} // Error
  if (true && (false || nullableO?.p === 'bar')) {} // Error
  if (true && (false || nullableX === 1)) {} // Error
  if (true && (false || nullableO?.p === 1)) {} // Error
}

function bool_literal_subtyping_check(b1: true, b2: false, b3: boolean) {
  if (b1 === true) {} // ok
  if (b1 === false) {} // error
  if (b1 !== true) {} // ok
  if (b1 !== false) {} // error

  if (b2 === true) {} // error
  if (b2 === false) {} // ok
  if (b2 !== true) {} // error
  if (b2 !== false) {} // ok

  if (b3 === true) {} // ok
  if (b3 === false) {} // ok
  if (b3 !== true) {} // ok
  if (b3 !== false) {} // ok
}
