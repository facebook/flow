<?hh
function print_res($c) {
  var_dump($c);
  echo("\n");
}
$v = Vector {};
print_res(empty($non_existent));
print_res(empty($v));
