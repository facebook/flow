<?hh
class Foo {
  public function __toString() {
    return "Foo";
  }
}
function verify_vector($v) {
  echo($v->isEmpty() ? "empty\n" : "not empty\n");
  echo($v->count()."\n");
  echo($v->at(1)."\n");
  echo($v[1]."\n");
  echo($v->get(1)."\n");
  try {
    $v->at(10);
    echo("should not see this");
  } catch (OutOfBoundsException $e) {
    echo($e->getMessage()."\n");
  }
  try {
    $v[10];
    echo("should not see this");
  } catch (OutOfBoundsException $e) {
    echo($e->getMessage()."\n");
  }
  echo(var_export($v->get(10), true)."\n");
  echo(isset($v[3]) ? "is set\n" : "not set\n");
  echo($v->containsKey(2) ? "contains Key\n": "does not contain Key\n");
  echo($v->containsKey(20) ? "contains Key\n": "does not contain Key\n");
  foreach ($v as $i => $val) {
    $out = var_export($val, true);
    echo("$i : $out\n");
  }
  $i = $v->getIterator();
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
$immVec = ImmVector {0, "My", true, new Foo()};
$vec = Vector {"zinc", 99, array(2 => 2)};
verify_vector($immVec);
verify_vector($vec);
$vec = $vec->add(20);
$vec[]= 40;
$vec[0] = "nonono";
$vec->set(1, "ribbit");
foreach ($vec as $val) {
  $out = var_export($val, true);
  echo($out." ");
}
echo("\n");
