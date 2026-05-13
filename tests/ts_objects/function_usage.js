// @flow
//
// Cross-file: `.js` consumer of the function declared in `function_lib.ts`.
// The `FunT -> Exact ObjT` relaxation is keyed on the consumer's file
// extension, so a `.js` consumer must still see the original error.

import {f} from './function_lib';

// Function into explicit-inexact `{...}` target: already accepted in `.js`.
f as {...}; // OK

// Function into exact target (`{}` is exact by default in `.js`): errors
// in `.js` consumer because the relaxation only fires for `.ts` consumers.
f as {}; // ERROR: function vs exact object

// Function into indexed target: errors here, and also still errors in a
// `.ts` consumer -- the indexed case is intentionally not relaxed.
f as {[k: string]: number}; // ERROR: function vs indexed object
