// Consuming an ambient (.d.ts) enum cross-module: uninitialized members are
// computed, not known literals, so the bare type is not `0 | 1` and members are
// not their ordinals.
import { E, F } from "./ambient";

// Computed members are typed as their representation (number), so the bare enum
// type is permissive rather than a false literal union. This matches TS: tsc also
// accepts `const e: E = 5` for an ambient enum with computed members, which is why
// modeling them this way avoids false positives on common .d.ts enums.
const e: E = 5; // OK
E.A satisfies number; // OK
E.A satisfies 0; // ERROR: A is computed, not the literal `0`

// An explicit initializer is still a known literal even in an ambient enum.
F.A satisfies 1; // OK
F.B satisfies number; // OK
F.B satisfies 2; // ERROR: B is computed in an ambient enum
