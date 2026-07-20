const s = 'foo' as const;

// The checker accepts this key because it resolves to the literal 'foo', but
// the signature builder cannot preserve the member. Rather than silently drop
// `foo` from the exported signature, the class fails signature verification,
// matching how exported object literals with computed keys behave.
export class C {
  [(s as 'foo')]: 1 = 1; // ERROR: cannot build a typed interface
}
