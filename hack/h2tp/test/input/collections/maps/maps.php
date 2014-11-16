<?hh
class Foo {
  public function __toString() {
    return "Foo";
  }
}
function verify_map($m) {
  echo($m->isEmpty() ? "empty\n" : "not empty\n");
  echo($m->count()."\n");
  echo($m->at("25")."\n");
  echo($m["25"]."\n");
  echo($m->get("25")."\n");
  try {
    $m->at(25);
    echo("should not see this");
  } catch (OutOfBoundsException $e) {
    echo($e->getMessage()."\n");
  }
  try {
    $m[25];
    echo("should not see this");
  } catch (OutOfBoundsException $e) {
    echo($e->getMessage()."\n");
  }
  echo(var_export($m->get(25), true)."\n");
  echo(isset($m["25"]) ? "is set\n" : "not set\n");
  echo($m->containsKey("25") ? "contains Key\n": "does not contain Key\n");
  echo($m->containsKey(25) ? "contains Key\n": "does not contain Key\n");
  foreach ($m as $i => $mal) {
    $out = var_export($mal, true);
    $t = gettype($i);
    echo("($t) $i : $out\n");
  }
  $i = $m->getIterator();
  $i->next();
  $i->next();
  $i->next();
  $i->next();
  try {
    $i->current();
    echo("should not see this");
  } catch (InvalidOperationException $e) {
    echo($e->getMessage()."\n");
  }
}
$immMap = ImmMap {"25" => "Galadriel", 9 => true, "9" => new Foo()};
$map = Map {"25" => "zinc", 9 => 99, "keyai" => array(2 => 2)};
verify_map($immMap);
verify_map($map);
$i = $map->getIterator();
$map["25"] = "copper";
echo("key is ".$i->key()."\n");
$map["logo"] = "basic";
try {
  $i->key();
  echo("should not see this");
} catch (InvalidOperationException $e) {
  echo($e->getMessage()."\n");
}
$map = $map->add(Pair {"tweedledee", "tweedledum"});
foreach ($map as $i => $mal) {
  $out = var_export($mal, true);
  $t = gettype($i);
  echo("($t) $i : $out -- ");
}
echo("\n");
