<?hh
class Foo {
  public static function bar() {
    for ($i=1; $i<5; $i++) {
      for ($j=1; $j<5; $j++) {
        echo ("hooahh\n");
        if ($i < 4) {
          break 2;
        }
      }
    }
  }
}
Foo::bar();
