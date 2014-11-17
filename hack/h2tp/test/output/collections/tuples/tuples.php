<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class Foo {
  private $c = array("hello", 5);
  private $y = "yoyo";
  private static $d = array(2, 3);
  public static $e = array(1, 5);
  public function bar() {
    if ((self::$d[0] + self::$d[1] + 1) === (self::$e[0] + self::$e[1])) {
      echo ("it's all good\n");
    }
    return $this->c;
  }
}
class Bar {
  private $x = array("hey ", "don't make "), $y = array("jude ", "it bad ");
  private $z;
  public function __construct() {
    $this->z = array("", "", "");
    $this->z[0] = $this->x[0].$this->y[0];
    $this->z[1] = $this->x[1].$this->y[1];
    $this->z[2] = $this->z[0].$this->z[1];
    echo ($this->z[2]."\n");
  }
}
class Baz extends Foo {}
$f = new Foo();
list($s, $i) = $f->bar();
echo ($s.$i."\n");
$g = new Baz();
list($s, $i) = $g->bar();
echo ($s.$i."\n");
new Bar();
