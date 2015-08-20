// Foo is a class-like function
function Foo() {
  this.x = 0; // constructs objects with property x
}
Foo.y = 0; // has static property y
Foo.prototype = { m() { return 0; } };

// exporting Foo directly often works
exports.Foo = Foo;

// but sometimes, you want to type Foo
// you could declare Foo as a class
declare class IFoo {
  x: boolean; // error, should have declared x: number instead
  static (): void;
  static y: boolean; // error, should have declared static y: number instead
}
exports.Foo2 = (Foo: Class<IFoo>);
