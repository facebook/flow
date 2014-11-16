<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'interfaces.php');
  require_once(__DIR__.SEP.'helper_traits'.SEP.'hacklib_immSetLike.php');
  require_once(__DIR__.SEP.'setIterator.php');

  /**
   * ImmSet is an immutable Set.
   */

  final class ImmSet implements \ConstSet, \ArrayAccess, \Stringish {
    use HACKLIB_ImmSetLike;
    /**
     * Create an ImmSet (if no parameters are passed) or create
     * an ImmSet from an Traversable (if one parameter is passed).
     */
    public function __construct($it = null) {
      $this->hacklib_init_t($it);
    }

    public function __toString() {
      return "ImmSet";
    }
  }
}
