<?hh
namespace {
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
  enum E1 : int {
    Blue = 12;
    Red = 15;
  }

  echo ("\nE1:\n");
  display (E1::assert(12));
  display (E1::assert(15));
  enum E2: mixed {
    Fly = "15";
    Blue = 12;
    Red = 15;
    Green = "green";
  }
  echo ("\nE2:\n");
  display (E2::assert(12));
  display (E2::assert("12"));
  display (E2::assert("15"));
  display (E2::assert(15));
  display (E2::assert("green"));
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
