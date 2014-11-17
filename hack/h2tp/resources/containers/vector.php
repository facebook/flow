<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'interfaces.php');
  require_once(__DIR__.SEP.'helper_traits'.SEP.'hacklib_vectorLike.php');
  require_once(__DIR__.SEP.'vectorIterator.php');

  /**
   * Vector is a stack-like collection.
   *
   * Like all objects in PHP, Vectors have reference-like semantics. When a
   * caller passes a Vector to a callee, the callee can modify the Vector and the
   * caller will see the changes. Vectors do not have "copy-on-write" semantics.
   *
   * Vectors only support integer keys. If a non-integer key is used, an
   * exception will be thrown.
   *
   * Vectors suoport "$m[$k]" style syntax for getting and setting values by
   * key. Vectors also support "isset($m[$k])" and "empty($m[$k])" syntax, and
   * they provide similar semantics as arrays. Elements can be added to a Vector
   * using "$m[] = .." syntax.
   *
   * Vectors do not support iterating while new elements are being added or
   * elements are being removed. When a new element is added or removed, all
   * iterators that point to the Vector shall be considered invalid.
   *
   * Vectors do not support taking elements by reference. If binding assignment
   * (=&) is used with an element of a Vector, or if an element of a Vector is
   * passed by reference, of if a Vector is used with foreach by reference, an
   * exception will be thrown.
   */

  final class Vector implements \MutableVector, \ArrayAccess, \Stringish {
    use HACKLIB_VectorLike;
    const MAX_SIZE = 3221225472;

    /**
     * Create an empty Vector (if no parameters are passed) or create
     * a Vector from an Traversable (if one parameter is passed).
     */
    public function __construct($it = null) {
      $this->hacklib_init_t($it);
    }

    public function __toString() {
      return "Vector";
    }

    public static function fromItems($items) {
      return new self($items);
    }

    public function immutable() {
      return $this->toImmVector();
    }

    public function contains($k) {
      return $this->containsKey($k);
    }

    public function append($i) {
      return $this->add($i);
    }

    /**
     * Returns a Vector built from the keys of the specified container.
     */
    public static function fromKeysOf($it) {
      if (is_array($it)) {
        return new self(array_keys($it));
      }
      if ($it instanceof \HH\KeyedIterable) {
        return new self($it->keys());
      }
      if (is_null($it)) {
        return new self();
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be a container (array or collection)');
      }
    }

    /**
     * used by HACKLIB_iteratable.
     * returns an iterator of the appropriate type
     */
    protected function hacklib_createNewIterator() {
      return new \VectorIterator();
    }
  }
}
