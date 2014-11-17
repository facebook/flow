<?hh
function foo($x) {
}
class Foo {
  public static function bar($str) {
    switch ($str) {
    case "specific":
      echo("in specific case");
      break;
    case "common_1":
    case "common_2":
      echo("in common case");
      break;
    case "fallthrough":
      echo("will fall through"); // FALLTHROUGH
    case "dummy":
      echo("dummy case");
      break;
    default:
      echo "I guess this is the end, my friend";
    }
    echo("\n");
    $y = true;
    $z = 41;
    switch (true) {
      case foo($str):
        echo ("helloooo");
        break;
      case $y && ($z + 1 == 42):
        echo ("nevermind");
        break;
    }
    echo("\n");
  }
}
Foo::bar("specific");
