// Sibling libdef that binds Foo into the value namespace. Pairing this
// `declare class Foo` with `type Foo` from a_types.js is rejected by the
// type_sig cross-namespace check, matching TypeScript's behavior.
declare class Foo {} // ERROR
