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
  display (E1::coerce(12));
  display (E1::coerce(15));
  enum E2: mixed {
    Fly = "15";
    Blue = 12;
    Red = 15;
    Green = "green";
  }
  echo ("\nE2:\n");
  display (E2::coerce(12));
  display (E2::coerce("12"));
  display (E2::coerce("15"));
  display (E2::coerce(15));
  display (E2::coerce("green"));
  display (E2::coerce(13));
  class Foo {
    public function __toString() {
      return "Foo";
    }
  }
  class Bar {}
  display(E2::coerce(new Foo()));
  display(E2::coerce(new Bar()));
  class Baz {
    public function __toString() {
      return 'green';
    }
  }
  display(E2::coerce(new Baz()));
}
