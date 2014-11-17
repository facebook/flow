<?hh
function foo(<<FOO, key("hello")>>string $foo) {
  return $foo." hello\n";
}
echo(foo("world"));
