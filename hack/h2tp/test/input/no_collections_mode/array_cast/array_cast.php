<?hh
function check_array_cast($c) {
  $arr = (array)$c;
  echo(is_array($arr) ? "YES\n" : "NO\n");
  echo(count($arr)."\n");
}
class Foo {}
check_array_cast(array(1, 2, 3));
check_array_cast(new Foo());
