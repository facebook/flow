<?hh
function lam(): int {
  $fn = $a ==> $a*2;
  $gn = $a ==> {
    $b = $a * 20;
    return $b*2;
  };
  return $fn(10) + $gn(10);
}
var_dump(lam());
