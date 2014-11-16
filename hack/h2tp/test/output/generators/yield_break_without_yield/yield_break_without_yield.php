<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function foo($start, $end) {
  if (false) {
    yield false;
  }
  return;
}
echo ("one\n");
$g = foo(1, 10);
foreach ($g as $k => $v) {
  echo ("$k = $v\n");
}
function bar($start, $end) {
  if (false) {
    yield false;
  }
  $f = function ($i) {
    yield $i;
  };
  $g = function () {
    yield true;
  };
  return;
}
echo ("two\n");
$g = bar(1, 10);
foreach ($g as $k => $v) {
  echo ("$k = $v\n");
}
