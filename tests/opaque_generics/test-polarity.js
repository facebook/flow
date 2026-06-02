class Foo<out T> {}

class Bar<in T> {}

class Baz<T> {}

class FooBarBaz<S, out T, in U> {}

opaque type Bad1<in T> = Foo<T>; // Error: Foo expects covariant type
opaque type Bad2<out T> = Bar<T>; // Error: Bar expects contravariant type
opaque type Bad3<in T> = Baz<T>; // Error: Baz expects invariant type
opaque type Bad4<out T> = Baz<T>; // Error: Baz expects invariant type

// Note: Invariant can flow to contravariant/covariant in the declarations.

export opaque type Covariant<out T> = Foo<T>;
export opaque type Contravariant<in T> = Bar<T>;
export opaque type Invariant<T> = Baz<T>;
export opaque type All<S,out T,in U> = FooBarBaz<S,T,U>;
