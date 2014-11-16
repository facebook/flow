<?hh
function &simple_ref() {
  static $ref = "stat";
  return $ref;
}
echo(simple_ref()."\n");
$adverb = &simple_ref();
$adverb = "now";
echo(simple_ref()."\n");
class Foo {
  public $x = "stat";
  public function &getX() {
    return $this->x;
  }
}
$foo = new Foo();
echo($foo->x."\n");
$foo_adverb = &$foo->getX();
$foo_adverb = "now";
echo($foo->x."\n");
