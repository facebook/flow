// When the class and interface declare the same field with different types,
// post-inference unification produces a MergedDeclaration error on the
// interface declaration line.
declare class C { x: number; }
interface C { x: string; } // ERROR

declare const c: C;
c.x as number;
c.x as string; // ERROR
