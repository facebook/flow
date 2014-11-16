<?php
// Copyright 2004-present Facebook. All Rights Reserved.
namespace HH {
  trait HACKLIB_ENUM_LIKE {
    private static $hacklib_names;
    private static $nonOverlappingValues = true;
    private static function hacklib_get_names() {
      if (!self::$hacklib_names) {
        $names = array();
        foreach (self::$hacklib_values as $key => $value) {
          if (!array_key_exists($value, $names)) {
            $names[$value] = $key;
          } else {
            self::$nonOverlappingValues = false;
          }
        }
        self::$hacklib_names = $names;
      }
      return self::$hacklib_names;
    }

    public static function isValid($v) {
      return (
        (is_int($v) || is_string($v)) &&
        array_key_exists($v, self::hacklib_get_names())
      );
    }

    public static function assert($v) {
      if (self::isValid($v)) {
        return $v;
      }
      throw new \UnexpectedValueException(
        "$v is not a valid value for ".get_called_class());
    }

    public static function coerce($v) {
      if (self::isValid($v)) {
        return $v;
      }
      return null;
    }

    public static function assertAll($traversable) {
      if (!(is_array($traversable) || $traversable instanceof \Traversable)) {
        $t = gettype($traversable);
        trigger_error("Argument 1 passed to HH\BuiltinEnum::assertAll() must ".
          "implement interface HH\Traversable, $t given", E_USER_ERROR);
      }
      $result = array();
      foreach ($traversable as $v) {
        $result[]= self::assert($v);
      }
      return $result;
    }

    public static function getNames() {
      $names = self::hacklib_get_names();
      invariant(self::$nonOverlappingValues, 'Enum has overlapping values');
      return $names;
    }

    public static function getValues() {
      return self::$hacklib_values;
    }
  }
}
