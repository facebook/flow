<?hh
class Foo {
  public function bar() {
    yield 5;
    yield break;
  }
  public static function baz() {
    yield break;
  }
}
foreach (Foo::baz() as $v) {
  echo($v."\n");
}
echo("break\n");
$f = new Foo();
foreach ($f->bar() as $v) {
  echo($v."\n");
}
