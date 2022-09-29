// @flow

// In case of infered type size violation, this should be annotated as `any`
function foo(x) {}

foo({f: 1, g: 2, h: 3});

// Hardcoded fixes should collapse the inferred union to a small size before annotation
// Should be annotated with `ObjType`, not `any`
function g(x) {}
type ObjType = {f: number, g: number, h: number};
const y: ObjType = {f: 1, g: 2, h: 3};
g(y);
g({f: 4, g: 5, h: 6});
