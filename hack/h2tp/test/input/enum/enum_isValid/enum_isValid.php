<?hh
enum E2: mixed {
  Fly = "15";
  Blue = 12;
  Red = 15;
  Green = "green";
}
var_dump(E2::isValid("15"));
var_dump(E2::isValid(15));
var_dump(E2::isValid(12));
var_dump(E2::isValid("12"));
var_dump(E2::isValid(13));
class Foo {}
var_dump(E2::isValid(new Foo()));
