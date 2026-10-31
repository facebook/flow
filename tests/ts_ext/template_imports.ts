import type { AttributeSelector, Selector, Keys } from "./template_palette";
import type { ParentSelector, Unbounded } from "./template_types";

// Imported selector aliases must retain their patterns across .d.ts and .ts signatures.
const attrGood: AttributeSelector = "&[data-x]"; // OK
const attrBad: AttributeSelector = "not-a-selector"; // ERROR
const parentGood: ParentSelector = "body &"; // OK
const parentBad: ParentSelector = "not-a-selector"; // ERROR
const selectorGood: Selector = "&[data-x]"; // OK
const selectorBad: Selector = "not-a-selector"; // ERROR

// Unbounded imported templates must constrain the literal prefix.
const unboundedGood: Unbounded = "prefix-value"; // OK
const unboundedBad: Unbounded = "wrong-value"; // ERROR

// Finite imported template unions must restrict the allowed keys.
const keyGood: Keys = "blue1"; // OK
const keyBad: Keys = "green1"; // ERROR
