<?hh
function everything_is_elephant($foo, &$bar, $baz) {
  $foo = $bar = $baz = "elephant";
}
$a = $b = $c = "giraffe";
everything_is_elephant($a, $b, $c);
echo("$a $b $c \n");
class Giraffe {
  public function changeMe(&$zee) {
    $zee = "giraffe";
  }
}
$o = "otter";
$g = new Giraffe();
$g->changeMe($o);
echo($o."\n");
$anon = function (&$x) {
  $x++;
};
$x = 20;
$anon($x);
echo($x."\n");
