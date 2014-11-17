<?hh
function foo($start, $end, $magic) {
  $i = $start;
  while ($i < $end) {
    if ($i === $magic) {
      yield break;
    }
    yield $i;
    $i++;
  }
}
$g = foo(1, 10, 4);
foreach ($g as $k => $v) {
  echo("$k = $v\n");
}
