<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'..'.SEP.'hacklib_iterator.php');
  require_once(__DIR__.SEP.'hacklib_commonContainerMethods.php');

  trait HACKLIB_ConstSetLike {
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
        'Only integer values and string values may be used with Sets');
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
        foreach ($it as $v) {
          $k_actual = self::hacklib_makeKey($v);
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
     * Returns true if the ImmSet is empty, false otherwise.
     */
    public function isEmpty() {
      return $this->count() == 0;
    }

    /**
     * Returns the number of elements in this ImmSet.
     */
    public function count() {
      return count($this->container);
    }

    /**
     * Returns true if the specified value is present in the ImmSet, returns
     * false otherwise.
     */
    public function contains($k) {
      return $this->hacklib_containsKey($k)[0];
    }

    /**
     *  identical to containsKey, implemented for ArrayAccess
     */
    public function offsetExists($offset) {
      return $this->hacklib_containsKey($offset)[0];
    }

    /**
     * implemented for ArrayAccess
     */
    public function offsetGet($offset) {
      list($contains, $k_actual) = $this->hacklib_containsKey($offset);
      if ($contains) {
        return $this->container[$k_actual];
      }
      if (is_int($offset)) {
        throw new \OutOfBoundsException("Integer key $offset is not defined");
      } else {
        if (strlen($offset) > 100) {
          $offset = "\"".substr($k, 0, 100)."\""." (truncated)";
        } else {
          $offset = "\"$offset\"";
        }
        throw new \OutOfBoundsException("String key $offset is not defined");
      }
    }

    /**
     * Returns an array containing the values from this ImmSet.
     */
    public function toArray() {
      $arr = array();
      foreach ($this as $k => $v) {
        if(isset($arr[$k])) {
          $type = join('', array_slice(explode('\\', __CLASS__), -1));
          trigger_error("$type::toArray() for a ".lcfirst($type).
            " containing both int($k) and string('$k')", E_USER_WARNING);
        }
        $arr[$k]= $v;
      }
      return $arr;
    }

    public static function fromItems($items) {
      return new self($items);
    }

    public static function fromArrays() {
      $args = func_get_args();
      $a = array();
      foreach ($args as $arg) {
        if (is_array($arg)) {
          foreach ($arg as $v) {
            $k_actual = self::hacklib_makeKey($v);
            $a[$k_actual] = $v;
          }
        } else {
        throw new \InvalidArgumentException(
          'Parameters must be arrays');
        }
      }
      $o = new self();
      $o->hacklib_setContainer($a);
      return $o;
    }

    /**
     * Returns a ConstSetLike built from the keys of the specified container.
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

    public function immutable() {
      return $this->toImmSet();
    }

    public function items() {
      return new \LazyIterableView($this);
    }

    /**
     * used by HACKLIB_iteratable.
     * returns the key and value at given index
     */
    protected function hacklib_getKeyAndValue($i) {
      $value = current(array_slice($this->container, $i, 1, true));
      return array($value, $value);
    }

    /**
     * used by HACKLIB_iteratable.
     * returns an iterator of the appropriate type
     */
    protected function hacklib_createNewIterator() {
      return new \SetIterator();
    }

    public function __debugInfo() {
      $a = array();
      foreach ($this->container as $key => $value) {
        if (is_string($key)) {
          $key = substr($key, 1);
        }
        $a[$key] = $value;
      }
      return $a;
    }
  }
}
