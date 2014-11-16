<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'hacklib_constSetLike.php');
  require_once(__DIR__.SEP.'hacklib_commonMutableContainerMethods.php');

  trait HACKLIB_SetLike {
    use HACKLIB_ConstSetLike;
    use HACKLIB_CommonMutableContainerMethods;

    public function offsetSet($offset, $value) {
      if (is_null($offset)) {
        $this->add($value);
      } else {
        throw new \InvalidOperationException(
          '[] operator cannot be used to modify elements of a Set');
      }
    }

    public function offsetUnset($offset) {
      $this->remove($offset);
    }

    /**
     * Adds an element to this Set and returns itself. "$c->add($v)" is
     * equivalent to "$c[] = $v" (except that add() returns the Set).
     */
    public function add($v) {
      list($contained, $k_actual) = $this->hacklib_containsKey($v);
      if (!$contained) {
        $this->hacklib_expireAllIterators();
      }
      $this->container[$k_actual] = $v;
      return $this;
    }

    /**
     * Adds the keys of the specified container to this Set and returns
     * the Set.
     */
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
     * Removes the specified value from this Set and returns itself.
     */
    public function remove($v) {
      $this->hacklib_expireAllIterators();
      $k_actual = self::hacklib_makeKey($v);
      unset($this->container[$k_actual]);
      return $this;
    }

    public function removeAll($it) {
      if (is_array($it) || $it instanceof \Traversable) {
        foreach ($it as $i) {
          $this->remove($i);
        }
        return $this;
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be an array or an instance of Traversable');
      }
    }

    public function retain($fn) {
      $this->container = array_filter(
        $this->container,
        function ($v) use($fn) {
          $c = $fn($v);
          if (!$c) {
            $this->hacklib_expireAllIterators();
          }
          return $c;
        });
      return $this;
    }

    public function retainWithKey($fn) {
      $this->container = array_filter(
        $this->container,
        function ($v, $k) use($fn) {
          $c = $fn(self::hacklib_unmakeKey($k), $v);
          if (!$c) {
            $this->hacklib_expireAllIterators();
          }
          return $c;
        }, ARRAY_FILTER_USE_BOTH);
      return $this;
    }
    protected function hacklib_isImmutable() {
      return false;
    }

    public function immutable() {
      return $this->toImmSet();
    }

    /**
     * Reserves enough memory to accommodate 'sz' elements. If 'sz' is less
     * than or equal to the current capacity of this MapLike, does nothing.
     */
    public function reserve($sz) {
      if ($sz < 0) {
        throw new \InvalidArgumentException(
          'Parameter sz must be a non-negative integer');
      }
      if ($sz > self::MAX_SIZE) {
        throw new \InvalidOperationException(
          "Set does not support reserving room for more than ".self::MAX_SIZE.
            " elements");
      }
      if ($sz > $this->count()) {
        $this->hacklib_expireAllIterators();
      }
      //do nothing for now. in the future we can store size separately and
      // pad the array
    }
  }
}
