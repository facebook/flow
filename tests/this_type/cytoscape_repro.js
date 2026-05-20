// Regression gate: this file should produce NO `[recursive-definition]`
// errors. Cyclic but `this`-free interface graph reduced from a libdef
// pattern in Cytoscape: `EdgeSingular extends Collection<EdgeSingular>`
// where `Collection<T = SingularReturn>` and
// `type SingularReturn = EdgeSingular`. Earlier iterations of the
// `this`-in-interfaces work tripped the recursion checker on graphs like
// this even though no `this` appears.

interface Collection<T = SingularReturn> {}
type SingularReturn = EdgeSingular;
export interface EdgeSingular extends Collection<EdgeSingular> {}

declare const x: EdgeSingular;
x as EdgeSingular; // OK
