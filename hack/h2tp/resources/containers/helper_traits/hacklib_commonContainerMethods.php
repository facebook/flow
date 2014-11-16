<?php
// Copyright 2004-present Facebook. All Rights Reserved.

namespace HH {
  require_once(__DIR__.SEP.'..'.SEP.'collections.ns.php');
  /**
   *  Methods that cane be implemented across all containers in one place. Either
   * these methods are implemented to actually locally account for differences
   * in these implementations, or is entirely common.
   */

  trait HACKLIB_CommonContainerMethods {
    /**
     *  This function is intended to be private. Do not call this directly.
     */
    public function __hacklib_getContainer() {
      return $this->container;
    }

    // This is used by some of the static creation methods to bypass
    // unnecessary iteration.
    private function hacklib_setContainer($container) {
      $this->container = $container;
    }

    public function __get($prop) {
      throw new \InvalidOperationException(
        'Cannot access a property on a collection');
    }

    public function __set($prop, $value) {
      throw new \InvalidOperationException(
        'Cannot access a property on a collection');
    }

    public function __isset($prop) {
      return false;
    }

    public function __unset($prop) {
      throw new \InvalidOperationException(
        'Cannot access a property on a collection');
    }

    public function hacklib_equals($other) {
      static $baseClass = array(
        'HH\Pair' => 'HH\Pair',
        'HH\Vector' => 'HH\Vector',
        'HH\ImmVector' => 'HH\Vector',
        'HH\Set' => 'HH\Set',
        'HH\ImmSet' => 'HH\Set',
        'HH\Map' => 'HH\Map',
        'HH\ImmMap' => 'HH\Map'
      );
      if (is_bool($other)) {
        return !($this->isEmpty()) == $other;
      }
      if (!is_object($other)) {
        return false;
      }
      // We know that our class is in this list.
      if ($baseClass[get_class($this)] != $baseClass[get_class($other)]) {
        return false;
      }
      $otherContainer = $other->__hacklib_getContainer();
      if (count($this->container) != count($otherContainer)) {
        return false;
      }
      foreach ($this->container as $key => $value) {
        if (
          !array_key_exists($key, $otherContainer) ||
          hacklib_not_equals($value, $otherContainer[$key])
        ) {
          return false;
        }
      }
      return true;
    }

    private static function hacklib_new_instance() {
      return new self();
    }

    // creates a new object of the same type. useful to implement
    // methods like map to operate in a general manner
    // the second optional option returns an ImmVector from a pair when
    // the method warrants it
    private function hacklib_create_new($it, $replacementForPair = null) {
      $obj = static::hacklib_new_instance();
      $obj->hacklib_init_t($it);
      return ($obj instanceof Pair && $replacementForPair) ?
        new $replacementForPair($obj) : $obj;
    }

    private static function hacklib_fnFromCallable($callable) {
      if (is_callable($callable)) {
        return function ($x) use ($callable) {
          return call_user_func($callable, $x);
        };
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be a valid callback');
      }
    }

    private static function hacklib_fn2FromCallable($callable) {
      if (is_callable($callable)) {
        return function ($x, $y) use ($callable) {
          return call_user_func($callable, $x, $y);
        };
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be a valid callback');
      }
    }
    public function lazy() {
      return new \LazyKeyedIterableView($this);
    }

    public function toVector() {
      return new Vector($this);
    }

    public function toImmVector() {
      return new ImmVector($this);
    }

    public function toSet() {
      return new Set($this);
    }

    public function toImmSet() {
      return new ImmSet($this);
    }

    public function toMap() {
      return new Map($this);
    }

    public function toImmMap() {
      return new ImmMap($this);
    }

    public function zip($traversable) {
      if (is_array($traversable)) {
        $traversable = new ImmVector($traversable);
      }
      if ($traversable instanceof \Traversable) {
        return $this->hacklib_create_new(
          new \LazyZipIterable($this, $traversable), "\\HH\\ImmVector");
      } else {
        throw new \InvalidArgumentException(
          'Parameter must be an array or an instance of Traversable');
      }
    }

    public function filter($fn) {
      return $this->hacklib_create_new(
        new \LazyFilterIterable($this, $fn), "\\HH\\ImmVector");
    }

    public function map($fn) {
      return $this->hacklib_map($fn);
    }

    private function hacklib_map($callable) {
      $fn = self::hacklib_fnFromCallable($callable);
      return $this->hacklib_create_new(
        new \LazyMapIterable($this, $fn));
    }

    /**
     * Returns an array whose values are this containers's keys.
     */
    public function toKeysArray() {
      return array_map(function ($k) {
        return static::hacklib_unmakeKey($k);
      }, array_keys($this->container));
    }

    public function toValuesArray() {
      return array_values($this->container);
    }

    public function mapWithKey($callable) {
      $fn = self::hacklib_fn2FromCallable($callable);
      return $this->hacklib_create_new(
        new \LazyMapWithKeyIterable($this, $fn), "\\HH\\ImmVector");
    }

    public function filterWithKey($callable) {
      $fn = self::hacklib_fn2FromCallable($callable);
      return $this->hacklib_create_new(
        new \LazyFilterWithKeyIterable($this, $fn), "\\HH\\ImmVector");
    }
    public function slice($start, $len) {
      if ($start < 0) {
        throw new \InvalidArgumentException(
          'Parameter start must be a non-negative integer');
      }
      if ($len < 0) {
        throw new \InvalidArgumentException(
          'Parameter len must be a non-negative integer');
      }
      return $this->hacklib_create_new(
        new \LazySliceIterable($this, $start, $len), "\\HH\\ImmVector");
    }

    // Note that pairs return a Vector despite the documentation saying
    // that they return an immVector
    public function take($n) {
      if (!is_int($n)) {
        throw new \InvalidArgumentException(
          'Parameter n must be an integer');
      }
      return $this->hacklib_create_new(
        new \LazyTakeIterable($this, $n), "\\HH\\Vector");
    }

    // Note that pairs return a Vector despite the documentation saying
    // that they return an immVector
    public function takeWhile($callable) {
      $fn = self::hacklib_fnFromCallable($callable);
      return $this->hacklib_create_new(
        new \LazyTakeWhileIterable($this, $fn), "\\HH\\Vector");
    }

    // Note that pairs return a Vector despite the documentation saying
    // that they return an immVector
    public function skip($n) {
      if (!is_int($n)) {
        throw new \InvalidArgumentException(
          'Parameter n must be an integer');
      }
      return $this->hacklib_create_new(
        new \LazySkipIterable($this, $n), "\\HH\\Vector");
    }

    // Note that pairs return a Vector despite the documentation saying
    // that they return an immVector
    public function skipWhile($callable) {
      $fn = self::hacklib_fnFromCallable($callable);
      return $this->hacklib_create_new(
        new \LazySkipWhileIterable($this, $fn), "\\HH\\Vector");
    }

    public function firstKey() {
      return static::hacklib_unmakeKey(key($this->container));
    }

    public function lastKey() {
      $last_item = array_slice($this->container, -1, 1, TRUE);
      return static::hacklib_unmakeKey(key($last_item));
    }

    public function firstValue() {
      if ($this->isEmpty()) {
        return null;
      }
      return current($this->container);
    }

    public function lastValue() {
      if ($this->isEmpty()) {
        return null;
      }
      $last_item = array_slice($this->container, -1, 1);
      return current($last_item);
    }

    protected abstract function hacklib_isImmutable();
  }
}
