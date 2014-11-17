<?hh
namespace foo\bar {
  trait t1 {
    public static $a = 20;
  }
  trait t2 {
    public static $b = Vector {"aa", "ee", "ii", "oo", "uu"};
  }
  trait t3 {
    use \foo\bar\t1;
    public static $c = Map {"uu" => "qq"};
  }
  class A {
    use t1;
  }
}
namespace foo {
  echo(bar\A::$a."\n");
  class B {
    use bar\t2, \foo\bar\t3;
  }
  echo (B::$c[B::$b[4]]."\n");
}
namespace {
  class C {
    use foo\bar\t3;
    public static $stuff = Vector {"uu"};
  }
  echo(C::$c[C::$stuff[0]]."\n");
}
