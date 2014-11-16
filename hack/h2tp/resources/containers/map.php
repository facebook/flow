<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'interfaces.php');
  require_once(__DIR__.SEP.'helper_traits'.SEP.'hacklib_mapLike.php');
  require_once(__DIR__.SEP.'mapIterator.php');

  /**
   * Map is an ordered dictionary-style collection.
   *
   * Like all objects in PHP, Maps have reference-like semantics. When a caller
   * passes a Map to a callee, the callee can modify the Map and the caller will
   * see the changes. Maps do not have "copy-on-write" semantics.
   *
   * Maps preserve insertion order of key/value pairs. When iterating over a Map,
   * the key/value pairs appear in the order they were inserted. Also, Maps do
   * not automagically convert integer-like string keys (ex. "123") into integer
   * keys.
   *
   * Maps only support integer keys and string keys. If a key of a different
   * type is used, an exception will be thrown.
   *
   * Maps support "$m[$k]" style syntax for getting and setting values by key.
   * Maps also support "isset($m[$k])" and "empty($m[$k])" syntax, and they
   * provide similar semantics as arrays. Adding an element using "$m[] = .."
   * syntax is not supported.
   *
   * Maps do not support iterating while new keys are being added or elements
   * are being removed. When a new key is added or an element is removed, all
   * iterators that point to the Map shall be considered invalid.
   *
   * Maps do not support taking elements by reference. If binding assignment (=&)
   * is used with an element of a Map, or if an element of a Map is passed by
   * reference, of if a Map is used with foreach by reference, an exception will
   * be thrown.
   */

  final class Map implements \MutableMap, \ArrayAccess, \Stringish {
    use HACKLIB_MapLike;
    const MAX_SIZE = 1610612736;
    /**
     * Create an empty Map (if no parameters are passed) or create
     * a Map from an KeyedTraversable (if one parameter is passed).
     */
    public function __construct($it = null) {
      $this->hacklib_init_t($it);
    }

    public function map($callback) {
      return $this->hacklib_map($callback);
    }

    public function __toString() {
      return "Map";
    }

    public static function hacklib_new($keys, $values) {
      $m = new Map();
      $m->hacklib_init_kv($keys, $values);
      return $m;
    }
  }
}
