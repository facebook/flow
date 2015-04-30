function foo() {
  { class Foo { }; }
  var fail = new Foo(); // error
}
