<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function check_array_cast($c) {
  $arr = (array) \hacklib_cast_as_array($c);
  echo (\hacklib_cast_as_boolean(is_array($arr)) ? "YES\n" : "NO\n");
  echo (count($arr)."\n");
}
check_array_cast(new \HH\Vector(array()));
check_array_cast(new \HH\ImmVector(array("hey", "few")));
check_array_cast(
  \HH\Map::hacklib_new(array("do", "doo"), array("be", "bee"))
);
