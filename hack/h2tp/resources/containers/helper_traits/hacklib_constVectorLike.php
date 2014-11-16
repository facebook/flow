<?php
// Copyright 2004-present Facebook. All Rights Reserved.
namespace HH {
  require_once(__DIR__.SEP.'..'.SEP.'hacklib_iterator.php');
  require_once(__DIR__.SEP.'hacklib_commonContainerMethods.php');

  /**
   * Trait to share behavior between containers that behave like vectors.
   * does not contain any implementation that is potentially mutable.
   */
  trait HACKLIB_ConstVectorLike {
    use HACKLIB_iteratable;
    use HACKLIB_CommonContainerMethods;
    //this is where we actually store elements
    private $container;

    protected function hacklib_init_t($it = null) {
      if (is_array($it) || $it instanceof \Traversable) {
        $a = array();
        foreach ($it as $v) {
          $a[]= $v;
        }
        $this->container = $a;
      } elseif (is_null($it)) {
        $this->container = array();
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be an array or an instance of Traversable');
      }
    }

    protected function hacklib_isBoundedKey($k) {
      return $k >= 0 && $k < $this->count();
    }

    protected function hacklib_validateKeyType($k) {
      if (!is_int($k)) {
        throw new \InvalidArgumentException(
          'Only integer keys may be used with '.(get_class($this)));
      }
    }

    protected function hacklib_validateKeyBounds($k) {
      if (!$this->hacklib_isBoundedKey($k)) {
        throw new \OutOfBoundsException("Integer key $k is out of bounds");
      }
    }

    /**
     * Returns true if the VectorLike is empty, false otherwise.
     */
    public function isEmpty() {
      return $this->count() === 0;
    }

    /**
     * Returns the number of elements in this VectorLike.
     */
    public function count() {
      return count($this->container);
    }

    /**
     * Returns the value at the specified key. If the key is not present,
     * an exception is thrown. "$v = $fvec->at($k)" is semantically equivalent
     * to "$v = $fvec[$k]".
     */
    public function at($k) {
      $this->hacklib_validateKeyType($k);
      $this->hacklib_validateKeyBounds($k);
      return $this->container[$k];
    }

    /**
     * Returns the value at the specified key. If the key is not present,
     * null is returned.
     */
    public function get($k) {
      $this->hacklib_validateKeyType($k);
      if ($this->hacklib_isBoundedKey($k)) {
        return $this->container[$k];
      }
      return null;
    }

    /**
     * Returns true if the specified key is present in the ImmVector, returns
     * false otherwise.
     */
    public function containsKey($k) {
      $this->hacklib_validateKeyType($k);
      return $k >= 0 && $k < $this->count();
    }

    /**
     *  identical to containsKey, implemented for ArrayAccess
     */
    public function offsetExists($offset) {
      return $this->containsKey($offset) && $this->at($offset) !== null;
    }

    /**
     * Returns an array containing the values from this VectorLike.
     */
    public function toArray() {
      $arr = array();
      foreach ($this as $v) {
        $arr[]= $v;
      }
      return $arr;
    }

    /**
     * Returns the index of the first element that matches the search value.
     * If no element matches the search value, this function returns -1.
     */
    public function linearSearch($search_value) {
      $k = array_search($search_value, $this->container, true);
      return $k === false ? -1 : $k;
    }

    public function items() {
      return new \LazyIterableView($this);
    }

    /**
     * used by HACKLIB_iteratable.
     * returns the key and value at given index
     */
    protected function hacklib_getKeyAndValue($i) {
      return array($i, $this->at($i));
    }

    public function __debugInfo() {
      return $this->container;
    }

    private static function hacklib_makeKey($k) {
      return $k;
    }

    private static function hacklib_unmakeKey($k){
      return $k;
    }
  }
}
