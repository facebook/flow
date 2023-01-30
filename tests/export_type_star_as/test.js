import type {aType as aType0} from "./forward_only";
("asdf": aType0); // ok
(42: aType0); // error

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
(42: aType1); // ok
("asdf": aType1); // error

// local exports override remote exports regardless of export order
import {type aType as aType2} from "./local_override2";
(42: aType2); // ok
("asdf": aType2); // error
