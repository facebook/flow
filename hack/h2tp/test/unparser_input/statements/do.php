<?hh
class Foo {
  public static function bar() {
    $i = 0;
    do {
      echo($i."\n");
    } while ($i > 0);
    do {
      break;
      echo ("should never occur\n");
    } while (0);
    do {
      $i++;
      echo($i."\n");
    } while ($i < 2);
  }
}
Foo::bar();
