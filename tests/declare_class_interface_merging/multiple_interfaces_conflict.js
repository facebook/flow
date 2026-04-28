// Two interfaces merging into the same declare class with conflicting fields:
// the first interface wins silently. The post-inference unification only
// compares class-vs-interface field types, not interface-vs-interface, so no
// MergedDeclaration error fires for this case.
declare class C { x: number; }
interface C { y: string; }
interface C { y: number; } // conflicts with first interface's y, silently dropped

declare const c: C;
c.y as string;
c.y as number; // ERROR (first interface wins; c.y is string not number, proving merge isn't 'any')
