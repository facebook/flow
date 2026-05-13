// .ts consumer: the gate kicks in (Files.has_ts_ext returns true for
// this file), so Box<Dog> -> Box<Animal> is accepted via the relaxed
// covariant treatment of Neutral tparams.

import {type Animal, type Dog, Box} from "./cross_lang_lib";

declare const dogBox: Box<Dog>;
const a: Box<Animal> = dogBox; // OK in .ts
