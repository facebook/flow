// In-file `declare class` skips the implicit-override (NIO) check via the
// `is_declare` flag set in `mk_declare_class_sig` — separate from the
// `under_declaration_context` carve-out (which only covers file-scope
// ambient like `.flow`, `.d.ts`, and `declare module`). Without the flag,
// declare-class members that shadow inherited members would be required
// to carry `override` even though they have no implementation body.

declare class Base {
  meth(): void;
  field: string;
}

declare class Sub extends Base {
  meth(): void; // OK — declare class skips NIO via is_declare
  field: string; // OK
}

// Explicit `override` still works on declare class (verified in
// tests/override/declare_class.js); NIO is what's suppressed here.
