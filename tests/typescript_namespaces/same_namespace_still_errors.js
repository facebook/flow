// Same-namespace duplicates are still errors — only cross-namespace
// coexistence is permitted.
var X1: string;
var X1 = 'value reassign'; // var redeclaration is JS-legal in same scope

let Y1: string;
let Y1 = 'second'; // ERROR: let cannot be redeclared

interface I1 {}
type I1 = number; // ERROR: name already bound in type namespace
