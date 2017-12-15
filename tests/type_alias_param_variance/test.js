// @flow

class Foo<T> {}
class Bar<+T> {}
class Baz<-T> {}

type A<-T> = Foo<T>; // Error, Foo expects invariant type
type B<+T> = Foo<T>; // Error, Foo expects invariant type
type C<T> = Foo<T>; // Ok!
type D<-T> = Bar<T>; // Error, Bar expects covariant type
type E<T> = Bar<T>; // Ok! (invariant can flow to anything)
type F<+T> = Bar<T>; // Ok!
type G<-T> = Baz<T>; // Ok!
type H<T> = Baz<T>; // Ok! (invariant can flow to anything)
type I<+T> = Baz<T>; // Error, Baz expects contravariant type
