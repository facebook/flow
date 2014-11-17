<?php
namespace foo {
  require_once ($GLOBALS['HACKLIB_ROOT']);
  class Foo {}
  new \HH\Vector();
  new \HH\ImmVector(array());
  new \HH\Map();
  new \HH\ImmMap(new \HH\Vector(array()));
  new \HH\Set();
  new \HH\ImmSet(null);
  new Foo();
}
namespace {
  new \HH\Vector();
  new \HH\ImmVector(array());
  new \HH\Map();
  new \HH\ImmMap(new \HH\Vector(array()));
  new \HH\Set();
  new \HH\ImmSet(null);
  new \foo\Foo();
}
