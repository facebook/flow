<?hh
class Foo {
  public static function bar() {
    while ($i < 5):
      echo($i);
      echo("\n");
      $i += 2;
    endwhile;
  }
}
Foo::bar();
