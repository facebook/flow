<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function myErrorHandler($errno, $errstr) {
  echo ("error $errstr");
  return true;
}
set_error_handler('myErrorHandler');
final class E2 {
  private function __construct() {
  }
  private static
    $hacklib_values = array(
      'Fly' => "15",
      'Blue' => 12,
      'Red' => 15,
      'Green' => "green"
    );
  use \HH\HACKLIB_ENUM_LIKE;
  const Fly = "15";
  const Blue = 12;
  const Red = 15;
  const Green = "green";
}
echo ("ImmVector: ");
var_dump(
  E2::assertAll(new \HH\ImmVector(array(15, "15", "12", 12, "green")))
);
echo ("array: ");
var_dump(E2::assertAll(array(15, "15", "12", 12, "green")));
echo ("Map: ");
var_dump(
  E2::assertAll(
    \HH\Map::hacklib_new(
      array(12, "1", 1, "grey", 3),
      array(15, "15", "12", 12, "green")
    )
  )
);
function fn() {
  $a = array(15, "15", "12", 12, "green");
  foreach ($a as $v) {
    yield $v;
  }
}
$g = fn();
echo ("Generator: ");
var_dump(E2::assertAll($g));
try {
  E2::assertAll(array(15, "true", 12));
} catch (\UnexpectedValueException $e) {
  echo ($e->getMessage()."\n");
}
var_dump(E2::assertAll("15"));
