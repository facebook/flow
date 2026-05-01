var someVar = 42;

class Foo {
  outer: number = someVar;
  selfTyped: Foo;
  selfTypedInit: Foo = new Foo();

  static outer: number = someVar;
  static selfTyped: Foo;
  static selfTypedInit: Foo = new Foo();

  constructor() {
    var someVar = 'asdf';
  }
}

/**
 * Field initializers execute in a scope immediately under the scope outside the
 * class definition.
 */
new Foo().outer as number;
new Foo().outer as string; // Error: number ~> string
Foo.outer as number;
Foo.outer as string; // Error: number ~> string

/**
 * Field initializers should be able to refer to the class type in their type
 * annotations.
 */
new Foo().selfTyped as Foo;
new Foo().selfTyped as number; // Error: Foo ~> number
Foo.selfTyped as Foo;
Foo.selfTyped as number; // Error: Foo ~> number

new Foo().selfTypedInit as Foo;
new Foo().selfTypedInit as number; // Error: Foo ~> number
Foo.selfTypedInit as Foo;
Foo.selfTypedInit as number; // Error: Foo ~> number
