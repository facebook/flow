<?php
require_once ($GLOBALS['HACKLIB_ROOT']);
trait DoNothing {}
class Foo {
  use DoNothing;
}
