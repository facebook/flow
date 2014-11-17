<?hh
class Foo {
  public static function bar() {
    for ($i=1; $i<5; $i++) {
      for ($j=1; $j<5; $j++) {
        if ($i*$j < 4) {
          continue 2;
        }
        $j = 4;
        echo ("Kilroy was here\n");
      }
    }
  }
}
Foo::bar();
