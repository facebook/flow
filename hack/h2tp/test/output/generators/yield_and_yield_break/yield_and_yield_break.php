<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function foo($start, $end, $magic) {
  $i = $start;
  while ($i < $end) {
    if ($i === $magic) {
      return;
    }
    yield $i;
    $i++;
  }
}
$g = foo(1, 10, 4);
foreach ($g as $k => $v) {
  echo ("$k = $v\n");
}
