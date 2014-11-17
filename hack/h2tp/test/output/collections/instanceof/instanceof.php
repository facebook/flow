<?php
namespace Boo {
  require_once ($GLOBALS['HACKLIB_ROOT']);
  interface Foo {}
  function somedata($c) {
    $t = gettype($c);
    if ($t === "object") {
      $t = get_class($c);
    }
    if ($c instanceof \HH\KeyedIterable) {
      echo ("$t is a KeyedIterable\n");
    }
    if (\hacklib_instanceof($c, 'HH\Traversable')) {
      echo ("$t is a Traversable\n");
    }
    if ($c instanceof \Traversable) {
      echo ("$t is a \Traversable\n");
    }
    if (\hacklib_instanceof($c, 'HH\Traversable')) {
      echo ("$t is a \HH\Traversable\n");
    }
    if ($c instanceof \HH\Iterator) {
      echo ("$t is an Iterator\n");
    }
    if ($c instanceof \HH\Iterator) {
      echo ("$t is an \HH\Iterator\n");
    }
    if ($c instanceof \Iterator) {
      echo ("$t is an Iterator\n");
    }
    if ($c instanceof \HH\Vector) {
      echo ("$t is a Vector\n");
    }
    $y = "HH\Traversable";
    if (\hacklib_instanceof($c, $y)) {
      echo ("$t is a \"HH\Traversable\"\n");
    }
    if ($c instanceof Foo) {
      echo ("$t is a Foo\n");
    }
    $y = "\Boo\Foo";
    if (\hacklib_instanceof($c, $y)) {
      echo ("$t is a \"\Boo\Foo\"\n");
    }
    echo ("\n\n");
  }
  class Bar implements Foo {}
  somedata(new \HH\Vector(array("c`est", "la", "vie")));
  somedata(\hacklib_id(new \HH\Vector(array()))->getIterator());
  somedata(array("so", "it", "goes"));
  somedata(new Bar());
}
