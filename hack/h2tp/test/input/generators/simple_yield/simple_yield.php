<?hh
function foo($start, $end) {
  $i = $start;
  while ($i < $end) {
    yield $i;
    $i++;
  }
}
function bar($start, $end) {
  $i = $start;
  while ($i < $end) {
    yield chr($i) => $i*2;
    $i++;
  }
}
$g = foo(1, 5);
foreach ($g as $k => $v) {
  echo("$k = $v\n");
}
$g = bar(97, 104);
foreach ($g as $k => $v) {
  echo("$k = $v\n");
}
