// @flow
//
// Cross-file: `.js` consumer of `take` declared in `extra_props_lib.ts`.
// The parameter type `{a: number}` is parsed under `exact_by_default` and
// becomes an exact ObjT. The Gate B `.ts`-only extra-prop relaxation does
// NOT apply in this `.js` consumer, so extra-prop errors still fire.

import {take} from './extra_props_lib';

// Source matching the param shape exactly: accepted.
take({a: 1}); // OK

// Source with an extra `b`: errors in `.js` consumer because the imported
// param type is exact and the relaxation is consumer-keyed.
take({a: 1, b: "extra"}); // ERROR: extra prop on exact target

// Missing the required `a`: still an error from the function call.
take({}); // ERROR: missing `a`

// Wrong-typed `a`: still an error.
take({a: "not a number"}); // ERROR: string vs number
