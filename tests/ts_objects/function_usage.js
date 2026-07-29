// @flow
//
// Cross-file: `.js` consumer of the function declared in `function_lib.ts`.
// The imported function retains TypeScript's exact-object compatibility.

import {f} from './function_lib';

// Function into explicit-inexact `{...}` target: already accepted in `.js`.
f as {...}; // OK

// Function into exact target (`{}` is exact by default in `.js`).
f as {}; // OK: function comes from .ts

// Function into indexed target: errors here, and also still errors in a
// `.ts` consumer -- the indexed case is intentionally not relaxed.
f as {[k: string]: number}; // ERROR: function vs indexed object
