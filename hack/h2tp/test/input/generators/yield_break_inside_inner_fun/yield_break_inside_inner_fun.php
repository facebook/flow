<?hh
function foo($start, $end, $magic) {
  $fn = function () use($start, $magic) {
    $i = $start;
    while ($i !== $magic - 1) {
      yield $i;
      $i++;
    }
    yield break;
  };
  $gn = () ==> {
    yield break;
  };
  foreach ($fn() as $v) {
    yield $v;
  }
  foreach ($gn() as $v) {
    yield $v;
  }
  yield break;
}
echo("one\n");
$g = foo(1, 10, 7);
foreach ($g as $k => $v) {
  echo("$k = $v\n");
}
