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
