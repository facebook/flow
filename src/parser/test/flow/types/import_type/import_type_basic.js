// Basic import type as module namespace
type M = import("module");

// Import with qualified access
type T = import("react").Component;

// Import with type arguments
type P = import("react").Provider<number>;

// Import with multiple qualifications
type D = import("lib").Outer.Inner;

// Import in function parameter type
function f(x: import("types").Param): void {}

// Import in variable type annotation
const x: import("config").Config = {};
