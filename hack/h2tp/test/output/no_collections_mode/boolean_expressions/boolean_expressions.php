<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
function foo($x) {
  if ($x) {
    echo ("truthy\n");
  } else {
    echo ("falsy\n");
  }
}
foo("non-null");
foo(null);
