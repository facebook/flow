<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function print_res($c) {
  var_dump($c);
  echo ("\n");
}
$v = new \HH\Vector(array());
print_res((!isset($non_existent)) || \hacklib_equals($non_existent, false));
print_res((!isset($v)) || \hacklib_equals($v, false));
