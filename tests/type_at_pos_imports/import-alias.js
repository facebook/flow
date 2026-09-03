// @flow

// An imported or exported name is an alias for a declaration elsewhere, and is
// framed as that declaration under an `(alias)` head. Non-callable value imports
// are immutable local bindings, so they are framed as `const`; `AF` is callable,
// so it is framed as a function.

import {AC} from './exports-alias';
//      ^
import {AE} from './exports-alias';
//      ^
import {AV} from './exports-alias';
//      ^
import {AS} from './exports-alias';
//      ^
import {AS as ASRenamed} from './exports-alias';
//      ^
import {AL as ALRenamed} from './exports-alias';
//      ^
import {AVar as AVarRenamed} from './exports-alias';
//      ^
import {AF} from './exports-alias';
//      ^
import type {AI} from './exports-alias';
//           ^
import type {AT} from './exports-alias';
//           ^
// A destructor in the aliased type makes hover print the evaluated form as well,
// pinning where the alias statement lands relative to it.
import type {AD} from './exports-alias-evaluated';
//           ^
import {AC as ACRenamed} from './exports-alias';
//            ^
import * as ANS from './exports-alias';
//          ^

type UseAT = AT;
//           ^
type UseAI = AI;
//           ^
const useAV: number = AV;
//                    ^

const local: number = 1;
export {local};
//      ^
export {local as localRenamed};
//               ^
