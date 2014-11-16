<?hh
function verify_pair($p) {
  echo($p->isEmpty() ? "empty\n" : "not empty\n");
  echo($p->count()."\n");
  echo($p->at(1)."\n");
  echo($p[1]."\n");
  echo($p->get(1)."\n");
  try {
    $p->at(10);
    echo("should not see this");
  } catch (OutOfBoundsException $e) {
    echo($e->getMessage()."\n");
  }
  try {
    $p[10];
    echo("should not see this");
  } catch (OutOfBoundsException $e) {
    echo($e->getMessage()."\n");
  }
  echo(var_export($p->get(10), true)."\n");
  echo(isset($p[3]) ? "is set\n" : "not set\n");
  echo($p->containsKey(2) ? "contains Key\n": "does not contain Key\n");
  echo($p->containsKey(20) ? "contains Key\n": "does not contain Key\n");
  foreach ($p as $i => $val) {
    $out = var_export($val, true);
    echo("$i : $out\n");
  }
  $i = $p->getIterator();
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
verify_pair(Pair {1, "hello"});
verify_pair(HH\Pair {1, "hello"});
verify_pair(\HH\Pair {1, "hello"});
