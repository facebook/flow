<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class Foo {
  public function __toString() {
    return "Foo";
  }
}
function verify_map($m) {
  echo
    (\hacklib_cast_as_boolean($m->isEmpty()) ? "empty\n" : "not empty\n")
  ;
  echo ($m->count()."\n");
  echo ($m->at("25")."\n");
  echo ($m[\hacklib_id("25")]."\n");
  echo ($m->get("25")."\n");
  try {
    $m->at(25);
    echo ("should not see this");
  } catch (OutOfBoundsException $e) {
    echo ($e->getMessage()."\n");
  }
  try {
    $m[25];
    echo ("should not see this");
  } catch (OutOfBoundsException $e) {
    echo ($e->getMessage()."\n");
  }
  echo (var_export($m->get(25), true)."\n");
  echo
    (\hacklib_cast_as_boolean(isset($m[\hacklib_id("25")]))
       ? "is set\n"
       : "not set\n")
  ;
  echo
    (\hacklib_cast_as_boolean($m->containsKey("25"))
       ? "contains Key\n"
       : "does not contain Key\n")
  ;
  echo
    (\hacklib_cast_as_boolean($m->containsKey(25))
       ? "contains Key\n"
       : "does not contain Key\n")
  ;
  foreach ($m as $i => $mal) {
    $out = var_export($mal, true);
    $t = gettype($i);
    echo ("($t) $i : $out\n");
  }
  $i = $m->getIterator();
  $i->next();
  $i->next();
  $i->next();
  $i->next();
  try {
    $i->current();
    echo ("should not see this");
  } catch (InvalidOperationException $e) {
    echo ($e->getMessage()."\n");
  }
}
$immMap = \HH\ImmMap::hacklib_new(
  array("25", 9, "9"),
  array("Galadriel", true, new Foo())
);
$map = \HH\Map::hacklib_new(
  array("25", 9, "keyai"),
  array("zinc", 99, array(2 => 2))
);
verify_map($immMap);
verify_map($map);
$i = $map->getIterator();
$map[\hacklib_id("25")] = "copper";
echo ("key is ".$i->key()."\n");
$map[\hacklib_id("logo")] = "basic";
try {
  $i->key();
  echo ("should not see this");
} catch (InvalidOperationException $e) {
  echo ($e->getMessage()."\n");
}
$map = $map->add(\HH\Pair::hacklib_new("tweedledee", "tweedledum"));
foreach ($map as $i => $mal) {
  $out = var_export($mal, true);
  $t = gettype($i);
  echo ("($t) $i : $out -- ");
}
echo ("\n");
