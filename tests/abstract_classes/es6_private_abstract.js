// `#private` members cannot be `abstract`: ES6 private names are nominally
// scoped to the declaring class and cannot be overridden by subclasses, so
// the obligation can never be discharged. Reject the combination at the
// parser (matches TS18019). Flow's parse-error reporting only surfaces the
// first parse error per file, so each case lives in its own file.
//
// Method case lives here; field case lives in [es6_private_abstract_field.js].

abstract class A {
  abstract #priv(): number; // ERROR: #private+abstract method
}
