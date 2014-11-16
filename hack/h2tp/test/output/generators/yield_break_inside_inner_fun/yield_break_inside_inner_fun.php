<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function foo($start, $end, $magic) {
  $fn = function () use ($start, $magic) {
    $i = $start;
    while ($i !== ($magic - 1)) {
      yield $i;
      $i++;
    }
    return;
  };
  $gn = function () {
    if (false) {
      yield false;
    }
    return;
  };
  foreach ($fn() as $v) {
    yield $v;
  }
  foreach ($gn() as $v) {
    yield $v;
  }
  return;
}
echo ("one\n");
$g = foo(1, 10, 7);
foreach ($g as $k => $v) {
  echo ("$k = $v\n");
}
