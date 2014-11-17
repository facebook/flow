<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'interfaces.php');
  require_once(__DIR__.SEP.'helper_traits'.SEP.'hacklib_immMapLike.php');
  require_once(__DIR__.SEP.'mapIterator.php');

  /**
   * ImmMap is an immutable Map.
   *
   * A ImmMap cannot be mutated. No elements can be added or removed from it,
   * nor can elements be overwritten using assignment (i.e. "$c[$k] = $v" is
   * not allowed).
   *
   * Construct it with a Traversable
   *
   *   $a = array('a' => 1, 'b' => 2);
   *   $fm = new ImmMap($a);
   *
   *   $fm = ImmMap::hacklib_new(array('a', 'b'),array(1, 2));
   *
   * Maps in Hack do not mix integer and string keys like regular php arrays.
   * To accommodate this, we prepend any key with a special string prefix,
   * "INT" when it's an int, and "STRING" when its a string.
   */

  final class ImmMap implements \ConstMap, \ArrayAccess, \Stringish {
    use HACKLIB_ImmMapLike;
    /**
     * Create an empty ImmMap (if no parameters are passed) or create
     * an ImmMap from an KeyedTraversable (if one parameter is passed).
     */
    public function __construct($it = null) {
      $this->hacklib_init_t($it);
    }

    public function map($callback) {
      return $this->hacklib_map($callback);
    }

    public function __toString() {
      return "ImmMap";
    }

    public static function hacklib_new($keys, $values) {
      $m = new ImmMap();
      $m->hacklib_init_kv($keys, $values);
      return $m;
    }
  }
}
