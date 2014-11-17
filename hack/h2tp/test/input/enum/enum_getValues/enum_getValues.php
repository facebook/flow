<?hh
enum E1: mixed {
  Fly = "15";
  Blue = 12;
  Red = 16;
  Green = "green";
}
var_dump(E1::getValues());
enum E2: mixed {
  Fly = "15";
  Blue = 12;
  Red = 15;
  Green = "green";
}
var_dump(E2::getValues());
