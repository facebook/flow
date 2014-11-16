<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'..'.SEP.'collections.ns.php');

  /**
   *  Methods that cane be implemented across all mutable containers in one
   * place. Either these methods are implemented to actually
   * locally account for differences
   * in these implementations, or is entirely common.
   */

  trait HACKLIB_CommonMutableContainerMethods {
    public function addAll($it) {
      if (is_array($it) || $it instanceof \Traversable) {
        foreach ($it as $v) {
          $this->add($v);
        }
        return $this;
      } elseif (is_null($it)) {
        return $this;
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be an array or an instance of Traversable');
      }
    }

    public function values() {
      return $this->toVector();
    }

    public function keys() {
      return new Vector(new \LazyKeysIterable($this));
    }

    public function concat($it) {
      if (is_array($it)) {
        $it = new ImmVector($it);
      }
      if ($it instanceof \Traversable) {
        return new Vector(
          new \LazyConcatIterable($this, $it));
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be an array or an instance of Traversable');
      }
    }

    /**
     * Removes all entries from this container
     */
    public function clear() {
      $this->hacklib_expireAllIterators();
      $old = $this->container;
      $this->container = array();
      unset($old);
      return $this;
    }

    public static function fromArray($arr) {
      return new self($arr);
    }
  }
}
