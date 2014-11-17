<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
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
  echo ("Operation $name : $e\n");
}
function boolean_expressions($c) {
  $t = \hacklib_cast_as_boolean(is_array($c)) ? "Array" : $c->__toString();
  $isEmpty =
    \hacklib_cast_as_boolean(is_array($c))
      ? ((!isset($c)) || \hacklib_equals($c, false))
      : $c->isEmpty();
  $isEmptyStr = \hacklib_cast_as_boolean($isEmpty) ? "Empty" : "Not Empty";
  echo ("\nTesting a $t that is $isEmptyStr \n");
  $b = (bool) \hacklib_cast_as_boolean($c);
  print_result("(bool) Cast", $b);
  $b = (boolean) \hacklib_cast_as_boolean($c);
  print_result("(boolean) Cast", $b);
  print_result("&& left", \hacklib_cast_as_boolean($c) && true);
  print_result("&& right", true && \hacklib_cast_as_boolean($c));
  print_result(
    "&& both",
    \hacklib_cast_as_boolean($c) && \hacklib_cast_as_boolean($c)
  );
  print_result("|| left", \hacklib_cast_as_boolean($c) || false);
  print_result("|| right", false || \hacklib_cast_as_boolean($c));
  print_result(
    "|| both",
    \hacklib_cast_as_boolean($c) || \hacklib_cast_as_boolean($c)
  );
  print_result("! ", !\hacklib_cast_as_boolean($c));
  print_result("Eif", \hacklib_cast_as_boolean($c) ? true : false);
}
function boolean_statements($c) {
  if (\hacklib_cast_as_boolean($c)) {
    echo ("If Then");
  } else {
    echo ("If Else");
  }
  $i = 0;
  do {
    $i++;
    if ($i > 3) {
      break;
    }
  }
  while(\hacklib_cast_as_boolean($c))
  ;
  echo ("Do Loop Iterations : $i\n");
  $i = 0;
  while (\hacklib_cast_as_boolean($c)) {
    $i++;
    if ($i > 3) {
      break;
    }
  }
  echo ("While loop Iterations : $i\n");
  $i = 0;
  for (; \hacklib_cast_as_boolean($c); $i++) {
    if ($i > 3) {
      break;
    }
  }
  echo ("For Loop iterations : $i\n");
}
function test_container($c) {
  boolean_expressions($c);
  boolean_statements($c);
}
function test_all_containers() {
  $data_arr = array(1, 2, 3);
  test_container(array());
  test_container($data_arr);
  test_container(new \HH\ImmVector(array()));
  test_container(new \HH\ImmVector($data_arr));
  test_container(new \HH\Vector(array()));
  test_container(new \HH\Vector($data_arr));
  test_container(\HH\Pair::hacklib_new(1, 2));
  test_container(new \HH\ImmSet(array()));
  test_container(new \HH\ImmSet($data_arr));
  test_container(new \HH\Set(array()));
  test_container(new \HH\Set($data_arr));
  test_container(\HH\ImmMap::hacklib_new(array(), array()));
  test_container(new \HH\ImmMap($data_arr));
  test_container(\HH\Map::hacklib_new(array(), array()));
  test_container(new \HH\Map($data_arr));
  test_container(new Foo());
}
test_all_containers();
