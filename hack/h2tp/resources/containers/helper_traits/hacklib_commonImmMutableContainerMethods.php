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

  trait HACKLIB_CommonImmMutableContainerMethods {
    public function keys() {
      return new ImmVector(new \LazyKeysIterable($this));
    }

    public function values() {
      return $this->toImmVector();
    }

    public function concat($it) {
      if (is_array($it)) {
        $it = new ImmVector($it);
      }
      if ($it instanceof \Traversable) {
        return new ImmVector(
          new \LazyConcatIterable($this, $it));
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be an array or an instance of Traversable');
      }
    }
  }
}
