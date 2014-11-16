<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'..'.SEP.'hacklib_iterator.php');
  require_once(__DIR__.SEP.'hacklib_commonContainerMethods.php');

  trait HACKLIB_ConstMapLike {
    use HACKLIB_iteratable;
    use HACKLIB_CommonContainerMethods;
    //this is where we actually store elements
    private $container;

    private static function hacklib_makeKey($k) {
      if (is_int($k)) {
        return $k;
      }
      if (is_string($k)) {
        return "_".$k;
      }
      else throw new \InvalidArgumentException(
        'Only integer keys and string keys may be used with Maps');
    }

    private static function hacklib_unmakeKey($k_actual) {
      if (is_string($k_actual)) {
        //cast to string because substr returns false if $k_actual = "_"
        return (string) substr($k_actual, 1);
      } else {
        return $k_actual;
      }
    }

    protected function hacklib_init_t($it = null) {
      if (is_array($it) || $it instanceof \Traversable) {
        $a = array();
        foreach ($it as $k => $v) {
          $k_actual = self::hacklib_makeKey($k);
          $a[$k_actual] = $v;
        }
        $this->container = $a;
      } elseif (is_null($it)) {
        $this->container = array();
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be an array or an instance of Traversable');
      }
    }

    protected function hacklib_init_kv(array $keys, array $values) {
      if (count($keys) !== count($values)) {
        throw new \InvalidArgumentException(
          get_class($this)." requires an equal number of keys and values.".
          "Received ".count($keys)." keys and ".count($values)." values ");
      }
      $a = array();
      foreach ($keys as $i => $k) {
        $k_actual = self::hacklib_makeKey($k);
        $a[$k_actual] = $values[$i];
      }
      $this->container = $a;
    }

    /**
     * Checks if the key is present in the map.
     * Returns the result AND the converted key for further
     * uses.
     */
    protected function hacklib_containsKey($k) {
      $k_actual = self::hacklib_makeKey($k);
      return array(array_key_exists($k_actual, $this->container), $k_actual);
    }

    /**
     * Returns true if the MapLike is empty, false otherwise.
     */
    public function isEmpty() {
      return $this->count() == 0;
    }

    /**
     * Returns the number of key/value pairs in the MapLike.
     */
    public function count() {
      return count($this->container);
    }

    /**
     * Returns the value at the specified key. If the key is not present,
     * an exception is thrown. "$v = $m->at($k)" is semantically equivalent
     * to "$v = $m[$k]".
     */
    public function at($k) {
      return $this[$k];
    }

    /**
     * Returns the value at the specified key. If the key is not present,
     * null is returned.
     */
    public function get($k) {
      list($contained, $k_actual) = $this->hacklib_containsKey($k);
      if ($contained) {
        return $this->container[$k_actual];
      }
      return null;
    }

     /**
     * Returns true if the specified key is present in the ImmMap, false
     * otherwise.
     */
    public function contains($k) {
      return $this->hacklib_containsKey($k)[0];
    }
    public function containsKey($k) {
      return $this->hacklib_containsKey($k)[0];
    }

    /**
     *  identical to containsKey, implemented for ArrayAccess
     */
    public function offsetExists($offset) {
      return $this->hacklib_containsKey($offset)[0] &&
        $this->at($offset) !== null;
    }

    /**
     * Returns an array containing the key/value pairs from this MapLike
     */
    public function toArray() {
      $arr = array();
      foreach ($this as $k => $v) {
        if (isset($arr[$k])) {
          $type = join('', array_slice(explode('\\', __CLASS__), -1));
          trigger_error("$type::toArray() for a ".lcfirst($type).
            " containing both int($k) and string('$k')", E_USER_WARNING);
        }
        $arr[$k]= $v;
      }
      return $arr;
    }

    /**
     * Returns a new Map with the keys in this Map not in $traversable
     */
    public function differenceByKey($it) {
      if ($it instanceof \Traversable) {
        $a = $this->container;
        foreach ($it as $k => $v) {
          $k_actual = self::hacklib_makeKey($k);
          unset($a[$k_actual]);
        }
        $newObj = static::hacklib_new_instance();
        $newObj->hacklib_setContainer($a);
        return $newObj;
      } else {
        throw new \InvalidArgumentException(
          'Parameter it must be an instance of Iterable');
      }
    }

    public function items() {
      return new \LazyKVZipIterable($this);
    }

    public static function fromItems($it) {
      if (is_array($it) || $it instanceof \Traversable) {
        $a = array();
        foreach ($it as $v) {
          if ($v instanceof Pair) {
            $k_actual = self::hacklib_makeKey($v[0]);
            $a[$k_actual] = $v[1];
          } else {
            throw new \InvalidArgumentException(
              'Parameter must be an array or an instance of Iterable<Pair>');
          }
        }
        $m = new self();
        $m->hacklib_setContainer($a);
        return $m;
      } elseif (is_null($it)) {
        return new self();
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be an array or an instance of Traversable');
      }

    }

    /**
     * used by HACKLIB_iteratable.
     * returns the key and value at given index
     */
    protected function hacklib_getKeyAndValue($i) {
      $keyAndValue = array_slice($this->container, $i, 1, true);
      return array(
        self::hacklib_unmakeKey(key($keyAndValue)),
        current($keyAndValue)
      );
    }

    /**
     * used by HACKLIB_iteratable.
     * returns an iterator of the appropriate type
     */
    protected function hacklib_createNewIterator() {
      return new \MapIterator();
    }

    public function __debugInfo() {
      $a = array();
      foreach ($this->container as $key => $value) {
        $k_actual = self::hacklib_unmakeKey($key);
        $a[$k_actual] = $value;
      }
      return $a;
    }
  }
}
