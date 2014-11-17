<?php
// Copyright 2004-present Facebook. All Rights Reserved.
namespace HH {
  require_once(__DIR__.SEP.'..'.SEP.'hacklib_iterator.php');
  require_once(__DIR__.SEP.'hacklib_constVectorLike.php');
  require_once(__DIR__.SEP.'hacklib_commonImmMutableContainerMethods.php');

  /**
   * Trait that ensures that all mutableish methods that are implemented by
   * immutable vectors throws errors.
   */
  trait HACKLIB_ImmVectorLike {
    use HACKLIB_ConstVectorLike;
    use HACKLIB_CommonImmMutableContainerMethods;

    /**
     * identical to at, implemented for ArrayAccess
     */
    public function offsetGet($offset) {
      $this->hacklib_validateKeyType($offset);
      $this->hacklib_validateKeyBounds($offset);
      return $this->container[$offset];
    }

    public function offsetSet($offset, $value) {
      throw new \InvalidOperationException(
        'Cannot modify immutable object of type '.get_class($this));
    }

    public function offsetUnset($offset) {
      throw new \InvalidOperationException(
        'Cannot modify immutable object of type '.get_class($this));
    }

    protected function hacklib_isImmutable() {
      return true;
    }
  }
}
