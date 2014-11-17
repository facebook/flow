<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function lam() {
  $fn = function ($a) {
    return $a * 2;
  };
  $gn = function ($a) {
    $b = $a * 20;
    return $b * 2;
  };
  return $fn(10) + $gn(10);
}
var_dump(lam());
