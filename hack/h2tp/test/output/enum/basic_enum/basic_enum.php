<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
final class DayOfWeek {
  private function __construct() {
  }
  private static
    $hacklib_values = array(
      'Sunday' => 0,
      'Monday' => 1,
      'Tuesday' => 2,
      'Wednesday' => 3,
      'Thursday' => 4,
      'Friday' => 5,
      'Saturday' => 6
    );
  use \HH\HACKLIB_ENUM_LIKE;
  const Sunday = 0;
  const Monday = 1;
  const Tuesday = 2;
  const Wednesday = 3;
  const Thursday = 4;
  const Friday = 5;
  const Saturday = 6;
}

function foo() {
  return DayOfWeek::Wednesday;
}
echo (foo()."\n");
