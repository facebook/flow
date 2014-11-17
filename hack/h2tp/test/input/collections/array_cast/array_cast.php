<?hh
function check_array_cast($c) {
  $arr = (array)$c;
  echo(is_array($arr) ? "YES\n" : "NO\n");
  echo(count($arr)."\n");
}
check_array_cast(Vector {});
check_array_cast(ImmVector {"hey", "few"});
check_array_cast(Map {"do" => "be", "doo" => "bee"});
