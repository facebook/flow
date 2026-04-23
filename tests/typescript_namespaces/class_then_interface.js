// `class C` and `interface C` coexist. The interface members aren't merged
// onto the class yet (declaration merging is a follow-up), but the bindings
// must coexist without raising name-already-bound.
class C { x: number = 1; }
interface C { y?: number; }

// Class is usable as a value (constructor) and as a type.
const c: C = new C();
