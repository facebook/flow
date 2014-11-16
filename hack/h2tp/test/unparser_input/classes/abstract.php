<?hh
abstract class Foo
{
   abstract protected function bar();
   public function baz() {}
   abstract public static function garply();
}

class Barrys extends Foo
{
  protected function bar() {}
  public static function garply() {}
}
