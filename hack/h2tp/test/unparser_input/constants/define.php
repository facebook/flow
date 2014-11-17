<?hh
  class C {
    public function __construct() {
      define("Foo", 10);
    }
  }
  function foo() {
    return "Foom";
  }
  define('GREETING', "Hello");
  define('Foom', foo());
  echo(GREETING);
  echo(Foom."\n");
  new C();
  echo Foo;
