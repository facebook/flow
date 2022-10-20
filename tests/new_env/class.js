//@flow

class A<T> { }

class B extends A<typeof D & C> { }

class C extends A<typeof D>{ }

class D {}
