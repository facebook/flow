<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
final class E1 {
  private function __construct() {
  }
  private static
    $hacklib_values = array(
      'Fly' => "15",
      'Blue' => 12,
      'Red' => 16,
      'Green' => "green"
    );
  use \HH\HACKLIB_ENUM_LIKE;
  const Fly = "15";
  const Blue = 12;
  const Red = 16;
  const Green = "green";
}
var_dump(E1::getValues());
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
var_dump(E2::getValues());
