<?hh
// in collections mode, all traits get an additional static method
trait DoNothing {
}
// in collections mode, any class using a trait gets an additional static
// method
class Foo {
  use DoNothing;
}
