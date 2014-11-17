<?hh
namespace Foo\Bar\Baz {
  class A {}
}
namespace {
  use Foo\Bar\Baz as Blah;
  $a = new Blah\A();
}
namespace Sue {
  use Foo\Bar\Baz;
  $a = new Baz\A();
}
