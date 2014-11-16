<?php
namespace blah {
  require_once ($GLOBALS['HACKLIB_ROOT']);
  $v = \HH\Vector::fromItems(array(1, 2, 3));
  echo ($v->count()."\n");
  $s = \HH\Set::fromKeysOf(\HH\Map::hacklib_new(array("abc"), array("123")));
  echo
    (\hacklib_cast_as_boolean($s->contains("abc"))
       ? "yes it does\n"
       : "no it doesnt\n")
  ;
}
namespace {
  $m = \HH\Map::fromArray(array("foo" => "bar"));
  echo ($m->count()."\n");
}
