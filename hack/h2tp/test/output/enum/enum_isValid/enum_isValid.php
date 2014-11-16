<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
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
var_dump(E2::isValid("15"));
var_dump(E2::isValid(15));
var_dump(E2::isValid(12));
var_dump(E2::isValid("12"));
var_dump(E2::isValid(13));
class Foo {}
var_dump(E2::isValid(new Foo()));
