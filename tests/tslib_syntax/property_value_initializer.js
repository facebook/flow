// Property value initializers should infer literal types when tslib_syntax=true
declare class Constants {
  static readonly BUFFER_SIZE = 256;
  readonly code = "NEXT_STATIC_GEN_BAILOUT";
  readonly MAX_SIZE = 30;
  readonly flag = true;
  readonly big = 100n;
  readonly neg = -1;
  readonly negBig = -100n;
}

// Verify literal type inference
declare const c: Constants;
c.code as "NEXT_STATIC_GEN_BAILOUT"; // OK
c.code as string; // OK (literal is subtype of string)
c.code as "wrong"; // ERROR - incompatible literal types
c.neg as -1; // OK
c.neg as 1; // ERROR - -1 !== 1
c.big as 100n; // OK
c.big as 0n; // ERROR - incompatible literal types
c.negBig as -100n; // OK
c.negBig as 100n; // ERROR - incompatible literal types

// Computed key with initializer
declare class ComputedInit {
  readonly [Symbol.iterator] = 0;
}

// Error: both annotation and init
declare class BothErr {
  readonly x: number = 42; // ERROR: DeclareClassPropertyAnnotationAndInit
}

// Error: non-readonly property with initializer
declare class NonReadonlyInit {
  x = 42; // ERROR: DeclareClassPropertyInitWithoutReadonly
}

// Error: non-literal initializer
declare class NonLiteralInit {
  readonly x = someVariable; // ERROR: DeclareClassPropertyNonLiteralInit
}

// Error: malformed field with neither : nor =
declare class MalformedField {
  x; // ERROR: DeclareClassPropertyMissingAnnotationOrInit
  readonly y; // ERROR: DeclareClassPropertyMissingAnnotationOrInit
}

// Private fields should get the same init validation
declare class PrivateInitErrors {
  private readonly x: number = 42; // ERROR: DeclareClassPropertyAnnotationAndInit
  private y = 42; // ERROR: DeclareClassPropertyInitWithoutReadonly
  private readonly z = someVariable; // ERROR: DeclareClassPropertyNonLiteralInit
}
