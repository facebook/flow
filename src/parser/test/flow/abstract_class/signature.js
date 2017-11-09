class Foo { abstract myMethod(SomeType): void; }
class Bar { abstract static myMethod(number): void; }
class Baz { abstract myMethod(SomeType); } // Error, Missing return type
