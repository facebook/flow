<?hh
class Foo {
  public function __toString() {
    return "Foo";
  }
  public function isEmpty() {
    return true;
  }
}
function print_result($name, $e) {
  $e = var_export($e, true);
  echo("Operation $name : $e\n");
}
function boolean_expressions($c) {
  $t = is_array($c) ? "Array" : $c->__toString();
  $isEmpty = is_array($c) ? empty($c) : $c->isEmpty();
  $isEmptyStr =  $isEmpty ? "Empty" : "Not Empty";
  echo("\nTesting a $t that is $isEmptyStr \n");
  $b = (bool)$c;
  print_result("(bool) Cast", $b);

  $b = (boolean)$c;
  print_result("(boolean) Cast", $b);

  print_result("&& left", $c && true);
  print_result("&& right", true && $c);
  print_result("&& both", $c && $c);
  print_result("|| left", $c || false);
  print_result("|| right", false || $c);
  print_result("|| both", $c || $c);
  print_result("! ", !$c);
  print_result("Eif", $c ? true : false);
}
function boolean_statements($c) {
  if ($c) {
    echo("If Then");
  } else {
    echo("If Else");
  }
  $i = 0;
  do {
    $i++;
    if ($i > 3) {
      break;
    }
  } while($c);
  echo("Do Loop Iterations : $i\n");
  $i = 0;
  while($c) {
    $i++;
    if ($i > 3) {
      break;
    }
  }
  echo("While loop Iterations : $i\n");
  $i = 0;
  for(; $c; $i++) {
    if ($i > 3) {
      break;
    }
  }
  echo("For Loop iterations : $i\n");
}
function test_container($c) {
  boolean_expressions($c);
  boolean_statements($c);
}
function test_all_containers() {
  $data_arr = array(1, 2, 3);
  test_container(array());
  test_container($data_arr);
  test_container(ImmVector{});
  test_container(new ImmVector($data_arr));
  test_container(Vector{});
  test_container(new Vector($data_arr));
  test_container(Pair {1, 2});
  test_container(ImmSet{});
  test_container(new ImmSet($data_arr));
  test_container(Set{});
  test_container(new Set($data_arr));
  test_container(ImmMap{});
  test_container(new ImmMap($data_arr));
  test_container(Map{});
  test_container(new Map($data_arr));
  test_container(new Foo());
}
test_all_containers();
