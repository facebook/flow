<?hh
function foo($start, $end) {
  yield break;
}
echo("one\n");
$g = foo(1, 10);
foreach ($g as $k => $v) {
  echo("$k = $v\n");
}
function bar($start, $end) {
  $f = function ($i) {
    yield $i;
  };
  $g = () ==> { yield true;};
  yield break;
}
echo("two\n");
$g = bar(1, 10);
foreach ($g as $k => $v) {
  echo("$k = $v\n");
}
