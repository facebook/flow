<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'interfaces.php');
  require_once(__DIR__.SEP.'helper_traits'.SEP.'hacklib_immVectorLike.php');
  require_once(__DIR__.SEP.'vectorIterator.php');

  /**
   * ImmVector is an immutable Vector.
   *
   * A ImmVector cannot be mutated. No elements can be added or removed from it,
   * nor can elements be overwritten using assignment (i.e. "$c[$k] = $v" is not
   * allowed).
   *
   *   $v = new Vector(array(1, 2, 3));
   *   $fv = $v->toImmVector();
   *
   * construct it with a Traversable
   *
   *   $a = array(1, 2, 3);
   *   $fv = new ImmVector($a);
   *
   */

  final class ImmVector implements \ConstVector, \ArrayAccess, \Stringish {
    use HACKLIB_ImmVectorLike;
    /**
     * Create an empty ImmVector (if no parameters are passed) or create
     * an ImmVector from an Traversable (if one parameter is passed).
     */
    public function __construct($it = null) {
      $this->hacklib_init_t($it);
    }

    public function __toString() {
      return "ImmVector";
    }

    public function immutable() {
      return $this;
    }
    /**
     * Returns an ImmVector built from the keys of the specified container.
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
