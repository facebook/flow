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

exports.a = 1; // Read of exports points to a fully resolved exports type.
