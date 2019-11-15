//@flow

import * as Types from "./type-exports.js"; // module with a type and non-type export
let Typologies = Types; // module through assignment

// mixed non-type and type imports
import { Typhoons, type Typesafety as Typesafe } from "./type-exports.js"

// typeof specifier import
import typeof { Typhoon } from "./type-exports.js";

// typeof default import
import typeof Typnotism from "./type-exports.js";

type Tyrant = string; // type declaration
opaque type Tympanic = number; // opaque type declaration

interface Typeset { // interface type
  foo : string;
}

let Tycoon = 1; // not a type

class Typewriter {}; // class type
let Typography = Typewriter; // class through assignment

function f<Typaram>() {
  declare var x :  // <-- AUTOCOMPLETE REQUEST HERE
}
