<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
class Foo {
  public function __toString() {
    return "Foo";
  }
}
function verify_set($s) {
  echo
    (\hacklib_cast_as_boolean($s->isEmpty()) ? "empty\n" : "not empty\n")
  ;
  echo ($s->count()."\n");
  echo
    (\hacklib_cast_as_boolean($s->contains("25"))
       ? "contains 25\n"
       : "does not contain 25\n")
  ;
  echo
    (\hacklib_cast_as_boolean($s->contains("truman"))
       ? "contains truman\n"
       : "does not contain truman\n")
  ;
  echo
    (\hacklib_cast_as_boolean(isset($s[\hacklib_id("25")]))
       ? "contains 25\n"
       : "does not contain 25\n")
  ;
  echo (gettype($s[\hacklib_id("25")])."\n");
  echo (gettype($s[\hacklib_id('25')])."\n");
  echo (gettype($s[25])."\n");
  try {
    $s[\hacklib_id("truman")];
    echo ("should not see this");
  } catch (OutOfBoundsException $e) {
    echo ($e->getMessage()."\n");
  }
  foreach ($s as $i => $val) {
    $out = var_export($val, true);
    $t = gettype($i);
    echo ("($t) $i : $out\n");
  }
  $i = $s->getIterator();
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
$immSet = new \HH\ImmSet(array("My", "25", 25, "My"));
$set = new \HH\Set(array("25", 25, "25", 25, "25", 25));
verify_set($immSet);
verify_set($set);
unset($set[\hacklib_id("25")]);
$s2 = $set->remove(25)->remove(25)->add(21)->add("21");
$s2[] = 300;
try {
  $s2[\hacklib_id("foo")] = "foo";
  echo ("should not see this");
} catch (InvalidOperationException $e) {
  echo ($e->getMessage()."\n");
}
echo (($s2 === $set) ? "Same Set\n" : "Not Same\n");
echo ($s2->count()."\n");
try {
  foreach ($s2 as $key => $value) {
    echo ($key."\n");
    $s2->add(2);
  }
} catch (InvalidOperationException $e) {
  echo ($e->getMessage()."\n");
}
