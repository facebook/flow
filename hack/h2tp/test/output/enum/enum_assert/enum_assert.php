<?php
namespace {
  require_once ($GLOBALS['HACKLIB_ROOT']);
  function myErrorHandler($errno, $errstr) {
    echo ("error $errno $errstr");
    return true;
  }
  set_error_handler('myErrorHandler');
}
namespace Meh {
  function display($i) {
    $t = gettype($i);
    echo ("($t) : $i\n");
  }
  final class E1 {
    private function __construct() {
    }
    private static $hacklib_values = array('Blue' => 12, 'Red' => 15);
    use \HH\HACKLIB_ENUM_LIKE;
    const Blue = 12;
    const Red = 15;
  }
  echo ("\nE1:\n");
  display(E1::assert(12));
  display(E1::assert(15));
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
  echo ("\nE2:\n");
  display(E2::assert(12));
  display(E2::assert("12"));
  display(E2::assert("15"));
  display(E2::assert(15));
  display(E2::assert("green"));
  try {
    E2::assert(13);
  } catch (\UnexpectedValueException $e) {
    echo ($e->getMessage()."\n");
  }
  class Foo {
    public function __toString() {
      return "Foo";
    }
  }
  class Bar {}
  try {
    E2::assert(new Foo());
  } catch (\UnexpectedValueException $e) {
    echo ($e->getMessage()."\n");
  }
  try {
    E2::assert(new Bar());
  } catch (\UnexpectedValueException $e) {
    echo ($e->getMessage()."\n");
  }
  class Baz {
    public function __toString() {
      return 'green';
    }
  }
  try {
    E2::assert(new Baz());
  } catch (\UnexpectedValueException $e) {
    echo ($e->getMessage()."\n");
  }
}
