/**
 * Fields annotated with a generic should assume a type once the type param
 * is instantiated.
 */
class ClassAnnotated<T> {
  p: T;
  static p: T;
}

var o1 = new ClassAnnotated<number>();
o1.p = 42;
o1.p as number;
o1.p as string; // Error: number ~> string
ClassAnnotated.p = 42;
ClassAnnotated.p as number;
ClassAnnotated.p as string; // Error: number ~> string


/**
 * It's always an error to initialized a generically-typed field with an
 * expression of any type other than the generic itself.
 */
class ClassGenericInitialized<T, U> {
  invalid: T = 42; // Error: number ~> Generic<T>
  valid: T = 42 as any as T;

  static invalid: T = 42; // Error: number ~> Generic<T>
  static valid: T = 42 as any as T;
}
