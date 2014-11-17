<?php
class Bar {}
class Foo {
  public function baz($x) {
    if (($x instanceof Foo) && true) {
      echo ("wheeee\n");
    }
    if ($x instanceof Bar) {
      echo ("TGIF\n");
    }
  }
}
$f = new Foo();
$f->baz($f);
