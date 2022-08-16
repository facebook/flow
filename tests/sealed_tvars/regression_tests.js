// @flow

function no_missing_env_entry_for_delete() {
  declare var foo: {bar: string | void};
  delete foo.bar; // ok
  (foo.bar: void); // ok
}

function no_missing_env_entry_for_illegal_type_binding() {
  type A = number;
  type A = number; // error, but no MissingEnvEntry internal error.
}

function unresolved_class_self_tvar() {
  class C { } // no weird unresolved tvar error
}

exports.a = 1; // Read of exports points to a fully resolved exports type.

class MissingAnnotations {
  a;
  b: number = 42;
  c = 42;
  d = 42 + 42;
  e = (x: number): number => x;
  f = (x: number) => { }
  h() {}
  i(): number { return 42 }
}

function f(x, {a, b}, ...y) { }

f((x) => x);
