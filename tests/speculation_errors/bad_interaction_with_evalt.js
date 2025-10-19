import type { Bad, Obj2 } from "./exported_bad_evalt";

declare const bad: Bad;
bad.get() as Obj2; // only subtyping error
bad.get(1) as Obj2; // ok
