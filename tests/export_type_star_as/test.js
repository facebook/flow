import type {aType as aType0} from "./forward_only";
"asdf" as aType0; // ok
42 as aType0; // error

import type {nope} from "./forward_only"; // error
import {aNum} from "./forward_only"; // error

import {
  type aType,
  type middleType,
  middleString,
} from "./forward_with_exports"; // ok
import {aNum as ignore} from "./forward_with_exports"; // error

// local exports override remote exports
import {type aType as aType1} from "./local_override1";
42 as aType1; // ok
"asdf" as aType1; // error

// local exports override remote exports regardless of export order
import {type aType as aType2} from "./local_override2";
42 as aType2; // ok
"asdf" as aType2; // error
