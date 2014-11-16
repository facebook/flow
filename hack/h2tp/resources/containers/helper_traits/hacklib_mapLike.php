<?php
// Copyright 2004-present Facebook. All Rights Reserved.
namespace HH {
  require_once(__DIR__.SEP.'hacklib_constMapLike.php');
  require_once(__DIR__.SEP.'hacklib_commonMutableContainerMethods.php');

  trait HACKLIB_MapLike {
    use HACKLIB_ConstMapLike;
    use HACKLIB_CommonMutableContainerMethods;

    /**
     * Stores a value into the Map with the specified key, overwriting any
     * previous value that was associated with the key. "$m->set($k,$v)" is
     * semantically equivalent to "$m[$k] = $v" (except that set() returns
     * the Map).
     */
    public function set($k, $v) {
      list($contained, $k_actual) = $this->hacklib_containsKey($k);
      if (!$contained) {
        $this->hacklib_expireAllIterators();
      }
      $this->container[$k_actual] = $v;
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
     * identical to at, implemented for ArrayAccess
     */
    public function &offsetGet($offset) {
      list($contained, $k_actual) = $this->hacklib_containsKey($offset);
      if ($contained) {
        return $this->container[$k_actual];
      }
      if (is_int($offset)) {
        throw new \OutOfBoundsException("Integer key $offset is not defined");
      } else {
        if (strlen($offset) > 100) {
          $offset = "\"".substr($offset, 0, 100)."\""." (truncated)";
        } else {
          $offset = "\"$offset\"";
        }
        throw new \OutOfBoundsException("String key $offset is not defined");
      }
    }

    public function offsetSet($offset, $value) {
      if (is_null($offset)) {
        $this->add($value);
      } else {
        $this->set($offset, $value);
      }
    }

    public function offsetUnset($offset) {
      list($contained, $k_actual) = $this->hacklib_containsKey($offset);
      if ($contained) {
        $this->hacklib_expireAllIterators();
        unset($this->container[$k_actual]);
      }
    }

    /**
     * Add a key/value Pair to this Map. "$mp->add($p)" is semantically
     * equivalent to "$mp[] = $p" (except that add() returns the Map).
     */
    public function add($p) {
      if (!($p instanceof Pair)) {
        throw new \InvalidArgumentException(
          'Parameter must be an instance of Pair');
      }
      list($k, $v) = $p;
      $this->set($k, $v);
      return $this;
    }

    /**
     * Removes the specified key from this Map.
     */
    public function remove($k) {
      $this->hacklib_expireAllIterators();
      $k_actual = self::hacklib_makeKey($k);
      unset($this->container[$k_actual]);
      return $this;
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

    public function removeKey($k) {
      return $this->remove($k);
    }

    protected function hacklib_isImmutable() {
      return false;
    }

    public function immutable() {
      return $this->toImmMap();
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
          "Map does not support reserving room for more than ".self::MAX_SIZE.
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
