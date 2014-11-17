<?hh
function foo($x, ...) {
  return $y[$x];
}
function bar(...) {
  return func_get_args()[3];
}
