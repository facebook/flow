<?hh
class A {
  public static $a = 25;
}
abstract class B extends A {
  private static $b = Vector {"do", "re", "mi"};
  public static function at($i) {
    return self::$b[$i];
  }
}
class C extends B {
  public static $c = Map {
    "do" => "a deer",
    "re" => "a drop",
    "mi" => "a name"
  };
  public static function at($i) {
    return static::$c[parent::at($i)];
  }
}
echo(C::at(1)."\n");
