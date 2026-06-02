class Foo<T> {}
class Bar<out T> {}
class Baz<in T> {}

type A<in T> = Foo<T>; // Error, Foo expects invariant type
type B<out T> = Foo<T>; // Error, Foo expects invariant type
type C<T> = Foo<T>; // Ok!
type D<in T> = Bar<T>; // Error, Bar expects covariant type
type E<T> = Bar<T>; // Ok! (invariant can flow to anything)
type F<out T> = Bar<T>; // Ok!
type G<in T> = Baz<T>; // Ok!
type H<T> = Baz<T>; // Ok! (invariant can flow to anything)
type I<out T> = Baz<T>; // Error, Baz expects contravariant type
