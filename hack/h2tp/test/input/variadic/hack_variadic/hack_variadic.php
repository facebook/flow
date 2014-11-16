<?hh
function var_foo(int $x, ...) : int {
  $arg_arr = func_get_args();
  return $x + count($arg_arr);
}
class Bar {
  public static function var_bar(...) : int {
    return count(func_get_args());
  }
}
function var_php_variadic($foo, ...$bar) {
  return count($bar);
}
echo(var_foo(5)."\n");
echo(var_foo(5, "hi", array(4, 2), null, true)."\n");
echo(Bar::var_bar(1, 2, 3)."\n");
echo(Bar::var_bar()."\n");
echo(var_php_variadic("10")."\n");
echo(var_php_variadic("10", 1, array())."\n");
