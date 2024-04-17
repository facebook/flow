import type { Bad, Obj2 } from "./exported_bad_evalt";

declare const bad: Bad;
bad.get() as Obj2; // error: TODO should not error here, since annotation error should be non-speculative
bad.get(1) as Obj2; // error: TODO spurious incompatible with empty
