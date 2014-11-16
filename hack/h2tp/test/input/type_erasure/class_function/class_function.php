<?hh

class C<T as IFoo> {
  public static function f<To>(
    @array<int, To> $foo,
    T $whatever
  ): @void {
    $x = ((int) (5.3 + 2.8)) + 20;
    echo($x);
  }
}

class D {
  public static function foo(
      Vector<int> $x,
      Map<int, string> $y
    ) : Vector<string> {
    return $x->map(function($i) use ($y) {
      return $y[$i];
    });
  }
}

C::f(array(), null);
