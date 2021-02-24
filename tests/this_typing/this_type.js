// @flow

class A {
  foo(this : this) {}
  static (this : this) {}
}

class B<T> {
  foo(this : this) {}
  static (this : this) {}
}

declare class C {
  foo(this : this) : void,
  static (this : this) : void
}

declare class D<T> {
  foo(this : this) : void,
  static (this : this) : void
}
