<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function foo($key) {
  echo (" looking for $key: \n");
  return $key." - hooah!";
}

class Bar {
  public function baz($index) {
    echo ("getting value at index $index\n");
    return $index ** 3;
  }
}
