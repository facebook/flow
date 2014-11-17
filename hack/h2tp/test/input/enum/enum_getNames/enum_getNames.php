<?hh
enum E1: mixed {
  Fly = "15";
  Blue = 12;
  Red = 16;
  Green = "green";
}
var_dump(E1::getNames());
enum E2: mixed {
  Fly = "15";
  Blue = 12;
  Red = 15;
  Green = "green";
}
try {
  E2::getNames();
} catch (HH\InvariantException $e) {
  echo($e->getMessage()."\n");
}
