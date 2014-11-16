<?php
class Foo {
  public function bar() {
    echo ("fooo\n");
  }
}
function test_dynamic_new() {
  $f = new Foo();
  $f->bar();
  $k = "Foo";
  $g = new $k();
  $g->bar();
}
test_dynamic_new();
