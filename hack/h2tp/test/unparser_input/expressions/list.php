<?hh
function foo($x) {
  list($a, $b) = $x;
  echo($a);
  echo($b);
  echo("\n");
}
foo(array("Lei ", "Minmei"));
