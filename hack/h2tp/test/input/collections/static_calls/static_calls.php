<?hh
namespace blah {
  $v = Vector::fromItems(array(1, 2, 3));
  echo($v->count()."\n");
  $s = \HH\Set::fromKeysOf(Map {"abc" => "123"});
  echo ($s->contains("abc") ? "yes it does\n" : "no it doesnt\n");
}
namespace {
  $m = HH\Map::fromArray(array("foo" => "bar"));
  echo($m->count()."\n");
}
