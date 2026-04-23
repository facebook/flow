// Mirror of namespace_then_class.js with the canonical class-first order.
// Already covered indirectly by tests/declare_namespace/typing_test.js but
// kept here so regressions show up locally in the typescript_namespaces test.
declare class L { lx: number; }
declare namespace L { type T = string; }

'' as L.T; // ok
1 as L.T; // err: number ~> string
new L(); // ok
