<?hh
$a = 10;
$b = &$a;
$b++;
echo("simple assignment ".$a."\n");
$c = array(1, 2, 3);
$d = &$c[1];
$d = "two";
echo("array get : ");
var_dump($c);
class E {
  public static $f = "f";
  public $g = "g";
}
$h = &E::$f;
$h = "h";
$e = new E();
$i = &$e->g;
$i = "i";
echo("class get ".E::$f."\n");
echo("object get ".$e->g."\n");
$j = 1;
$k = array(2, 3);
$l = array(&$j, &$k[0], &$k[1]);
$l[0]++; $l[1]++; $l[2]++;
echo("Ref used inside array: $j\n");
var_dump($k);
$m = array("r" => "x", "s" => "y", "t" => "z");
foreach ($m as $key => &$value) {
  $value = $value." KEK";
}
echo ("Ref in the bound expr of foreach: ");
var_dump($m);
