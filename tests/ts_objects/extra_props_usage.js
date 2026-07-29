// @flow
//
// Cross-file: `.js` consumer of `take` declared in `extra_props_lib.ts`.
// The imported parameter retains TypeScript's relaxed exact-object semantics.

import {take} from './extra_props_lib';

// Source matching the param shape exactly: accepted.
take({a: 1}); // OK

// Source with an extra `b`: errors in `.js` consumer because the imported
// param type is exact and the relaxation is consumer-keyed.
take({a: 1, b: "extra"}); // OK: target comes from .ts

// Missing the required `a`: still an error from the function call.
take({}); // ERROR: missing `a`

// Wrong-typed `a`: still an error.
take({a: "not a number"}); // ERROR: string vs number
