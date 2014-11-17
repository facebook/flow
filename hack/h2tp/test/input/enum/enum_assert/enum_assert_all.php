<?hh
function myErrorHandler($errno, $errstr) {
  echo ("error $errstr");
  return true;
}
set_error_handler('myErrorHandler');
enum E2: mixed {
  Fly = "15";
  Blue = 12;
  Red = 15;
  Green = "green";
}
echo("ImmVector: ");
var_dump(E2::assertAll(ImmVector {15, "15", "12", 12, "green"}));
echo("array: ");
var_dump(E2::assertAll(array(15, "15", "12", 12, "green")));
echo("Map: ");
var_dump(E2::assertAll(
  Map {12 => 15, "1" => "15", 1 => "12", "grey" => 12, 3 => "green"}));
function fn() {
  $a = array(15, "15", "12", 12, "green");
  foreach ($a as $v) {
    yield $v;
  }
}
$g = fn();
echo("Generator: ");
var_dump(E2::assertAll($g));
try {
  E2::assertAll(array(15, "true", 12));
} catch (\UnexpectedValueException $e) {
  echo ($e->getMessage()."\n");
}
var_dump(E2::assertAll("15"));
