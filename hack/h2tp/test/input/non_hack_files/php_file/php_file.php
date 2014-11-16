<?php


class C {
  public static function f(
    $foo,
    $whatever
  ) {}
}

function g( $bar) {}

function h() {
  return GLOBAL_FOO;
}

class D {
  public static function foo(
      $x,
      $y
    ) {
    return $x->map(function($i) use ($y) {
      return $y[$i];
    });
  }
}

C::f(null, null);
g(null);
