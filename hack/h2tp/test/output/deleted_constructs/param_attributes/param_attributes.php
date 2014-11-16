<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function foo($foo) {
  return $foo." hello\n";
}
echo (foo("world"));
