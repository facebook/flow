declare namespace conflict_with_type_decl_1 {}
type conflict_with_type_decl_1 = 1; // error
type conflict_with_type_decl_2 = 1;
declare namespace conflict_with_type_decl_2 {} // error

declare namespace const_like {}
declare namespace const_like {} // error
const_like = 1; // error: reference-before-declaration of the const below
const const_like = 1; // ok: empty-body namespace is type-only, const is in the value namespace

allow_forward_ref as {...}; // ok
declare namespace allow_forward_ref {
  declare const foo: number;
}
