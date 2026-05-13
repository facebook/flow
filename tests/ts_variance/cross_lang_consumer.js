// @flow
// .js consumer of the same .ts-defined generic. The gate keys on
// Files.has_ts_ext (Context.file cx), so a .js context stays strict --
// Box<Dog> -> Box<Animal> still errors here, even though the same flow
// is accepted from a .ts consumer in cross_lang_consumer.ts.

import {type Animal, type Dog, Box} from "./cross_lang_lib";

declare const dogBox: Box<Dog>;
const a: Box<Animal> = dogBox; // ERROR in .js: Neutral T is invariant
