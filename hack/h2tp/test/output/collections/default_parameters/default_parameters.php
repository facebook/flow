<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function foo($v = \HACKLIB_UNINIT, $sep = ",") {
  if ($v === \HACKLIB_UNINIT) {
    $v = new \HH\Vector(array(1, 2, 3));
  }
  $s = (array) \hacklib_cast_as_array($v);
  echo (join($sep, $s)."\n");
}
function fonz(
  $v = \HACKLIB_UNINIT,
  $s = \HACKLIB_UNINIT,
  $m = \HACKLIB_UNINIT
) {
  if ($v === \HACKLIB_UNINIT) {
    $v = new \HH\Vector(array());
  }
  if ($s === \HACKLIB_UNINIT) {
    $s = new \HH\Set(array());
  }
  if ($m === \HACKLIB_UNINIT) {
    $m = \HH\Map::hacklib_new(array(), array());
  }
}
class Bar {
  public $s;
  public function __construct($s = \HACKLIB_UNINIT) {
    if ($s === \HACKLIB_UNINIT) {
      $s = new \HH\Set(array("witches", "wizards"));
    }
    $this->s = $s;
  }
}
foo();
foo(new \HH\Set(array(1, "hi", 1)), " : ");
$b = new Bar();
foo($b->s, " - ");
foo(
  \hacklib_id(
    new Bar(\HH\Map::hacklib_new(array("x", "p"), array("y", "q")))
  )->s,
  " = "
);
interface Boo {
  public function fizz($v = \HACKLIB_UNINIT);
}
abstract class Booze {
  abstract public function buzz($s = \HACKLIB_UNINIT);
}
class BooImpl extends Booze implements Boo {
  public function fizz($v = \HACKLIB_UNINIT) {
    if ($v === \HACKLIB_UNINIT) {
      $v = new \HH\Vector(array(1, 2));
    }
    echo (join(", ", (array) \hacklib_cast_as_array($v))."\n");
  }
  public function buzz($s = \HACKLIB_UNINIT) {
    if ($s === \HACKLIB_UNINIT) {
      $s = \HH\Map::hacklib_new(array('a'), array('z'));
    }
    echo (join(", ", (array) \hacklib_cast_as_array($s))."\n");
  }
}
function doIt($i) {
  $i->fizz();
  if ($i instanceof Booze) {
    $i->buzz();
  }
}
doIt(new BooImpl());
