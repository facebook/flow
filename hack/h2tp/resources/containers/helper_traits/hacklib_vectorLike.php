<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'..'.SEP.'hacklib_iterator.php');
  require_once(__DIR__.SEP.'hacklib_constVectorLike.php');
  require_once(__DIR__.SEP.'hacklib_commonMutableContainerMethods.php');

  /**
   * Trait that implements all mutating methods of Vectors.
   */
  trait HACKLIB_VectorLike {
    use HACKLIB_ConstVectorLike;
    use HACKLIB_CommonMutableContainerMethods;

    /**
     * identical to at, implemented for ArrayAccess
     */
    public function &offsetGet($offset) {
      $this->hacklib_validateKeyType($offset);
      $this->hacklib_validateKeyBounds($offset);
      return $this->container[$offset];
    }

    public function offsetSet($offset, $value) {
      if (is_null($offset)) {
        $this->add($value);
      } else {
        $this->set($offset, $value);
      }
    }

    public function offsetUnset($offset) {
      throw new \RuntimeException(
        'Cannot unset an element of a '.get_class($this));
    }

   /**
     * Stores a value into the Vector with the specified key, overwriting the
     * previous value associated with the key. If the key is not present,
     * an exception is thrown. "$vec->set($k,$v)" is semantically equivalent
     * to "$vec[$k] = $v" (except that set() returns the Vector).
     */
    public function set($k, $v) {
      $this->hacklib_validateKeyType($k);
      $this->hacklib_validateKeyBounds($k);
      $this->container[$k] = $v;
      return $this;
   }

    public function setAll($it) {
      if (is_array($it) || $it instanceof \Traversable) {
        foreach ($it as $k => $v) {
          $this->set($k, $v);
        }
        return $this;
      } elseif (is_null($it)) {
        return $this;
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be an array or an instance of Traversable');
      }
   }

    /**
     * Append a copy of a value to the end of the Vector, assigning the next
     * available integer key. "$vec->add($v)" is semantically equivalent to
     * "$vec[] = $v" (except that add() returns the Vector).
     */
    public function add($value) {
      $this->hacklib_expireAllIterators();
      $this->container[]= $value;
      return $this;
   }

    /**
     * Adds the keys of the specified container to this Vector and returns
     * the Vector.
     */
    public function addAllKeysOf($it) {
      if (is_array($it) || $it instanceof \Traversable) {
        foreach ($it as $k => $v) {
          $this->add($k);
        }
        return $this;
      } elseif (is_null($it)) {
        return $this;
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be a container (array or collection)');
      }
   }

    /**
     * Removes the specified key from this Vector. This will cause elements
     * with higher keys to be renumbered.
     */
    public function removeKey($k) {
      if ($k >= 0 && $k < count($this->container)) {
        array_splice($this->container, $k, 1);
        $this->hacklib_expireAllIterators();
      }
      return $this;
   }

    /**
     * Remove the last element of this Vector and return it. This function
     * throws an exception if this Vector is empty.
     */
    public function pop() {
      if (!$this->container) {
        throw new \InvalidOperationException('Cannot pop empty '.get_class($this));
      }
      $this->hacklib_expireAllIterators();
      return array_pop($this->container);
   }

    /**
     * Resize this Vector to contain 'sz' elements. If 'sz' is smaller than
     * the current size of this Vector, elements are removed from the end of
     * this Vector. If 'sz' is greater than the current size of this Vector,
     * this Vector is extended by appending as many copies of 'value' as
     * needed to reach a size of 'sz' elements.
     */
    public function resize($sz, $value) {
      if ($sz > self::MAX_SIZE) {
        throw new \InvalidArgumentException(
          "Parameter sz must be at most ".self::MAX_SIZE."; $sz passed");
      }
      if ($this->count() === $sz) {
        return;
      }
      $this->hacklib_expireAllIterators();
      if ($this->count() > $sz) {
        array_splice($this->container, $sz);
      } else {
        $this->container = array_pad($this->container, $sz, $value);
      }
   }

    /**
     * Reserves enough memory to accommodate 'sz' elements. If 'sz' is less
     * than or equal to the current capacity of this VectorLike, does nothing.
     */
    public function reserve($sz) {
      if ($sz < 0) {
        throw new \InvalidArgumentException(
          'Parameter sz must be a non-negative integer');
      }
      if ($sz > self::MAX_SIZE) {
        throw new \InvalidArgumentException(
          "Parameter sz must be at most ".self::MAX_SIZE."; $sz passed");
      }
      if ($sz > $this->count()) {
        $this->hacklib_expireAllIterators();
      }
      // do nothing for now. in the future we can store size separately and
      // pad the array
    }

    /**
     * Reverse the elements of this Vector in place.
     */
    public function reverse() {
      $this->container = array_reverse($this->container);
      if ($this->count() > 1) {
        $this->hacklib_expireAllIterators();
      }
   }

    /**
     * Splice this Vector in place. This function provides the functionality
     * of array_splice() for Vectors. Note that this function modifies this
     * Vector in place.
     */
    public function splice($offset, $len = null) {
      if (!is_int($offset)) {
        throw new \InvalidArgumentException(
          'Parameter offset must be an integer');
      }
      if (!is_null($len) && !is_int($len)) {
        throw new \InvalidArgumentException(
          'Parameter len must be null or an integer');
      }
      $removed = is_null($len) ? array_splice($this->container, $offset) :
        array_splice($this->container, $offset, $len);
      if (count($removed) > 0) {
        $this->hacklib_expireAllIterators();
      }
   }

    /**
     * Shuffles the values of the Vector randomly in place.
     */
    public function shuffle() {
      shuffle($this->container);
      if ($this->count() > 1) {
        $this->hacklib_expireAllIterators();
      }
   }

    protected function hacklib_isImmutable() {
      return false;
    }
  }
}
