<?php
// Copyright 2004-present Facebook. All Rights Reserved.

const SEP = DIRECTORY_SEPARATOR;
require_once(__DIR__.SEP.'containers'.SEP.'interfaces.php');
require_once(__DIR__.SEP.'containers'.SEP.'collections.ns.php');
require_once(__DIR__.SEP.'containers'.SEP.'vector.php');
require_once(__DIR__.SEP.'containers'.SEP.'immVector.php');
require_once(__DIR__.SEP.'containers'.SEP.'pair.php');
require_once(__DIR__.SEP.'containers'.SEP.'map.php');
require_once(__DIR__.SEP.'containers'.SEP.'immMap.php');
require_once(__DIR__.SEP.'containers'.SEP.'set.php');
require_once(__DIR__.SEP.'containers'.SEP.'immSet.php');
require_once(__DIR__.SEP.'hacklib_enum_like.php');
require_once(__DIR__.SEP.'invariant.ns.php');


class InvalidOperationException extends RuntimeException {}

// This invalid UTF-8 subsequence prevents users from entering this string
// to prevent accidental collisions.
const HACKLIB_UNINIT = "<<hacklib-uninitialized:\xEE\xFF\xFF>>";

/**
 *  Identity function. Mostly useful idiomatically, because this doesn't
 *  compile:
 *
 *    new ClassName()->methodName();
 *
 *  ...but this works fine:
 *
 *    id(new ClassName())->method();
 *
 *  @param    Value to return.
 *  @return   The passed value.
 */
function hacklib_id($x) {
  return $x;
}

/**
 * Abstracts patterns similar to:
 *   $r = null;
 *   if ($x) {
 *     $r = $x->y();
 *   }
 * or
 *  if ($v) {
 *    $v->doSomething();
 *  }
 *
 * to be
 *   $r = nullsafe($x)->y();
 * or
 *   nullsafe($v)->doSomething();
 *
 * Another way to think of it is a kind of idx() for objects.
 */
function hacklib_nullsafe($v) {
  if ($v === null) {
    return HACKLIB_NullObject::getInstance();
  } else {
    return $v;
  }
}

/**
 * A lot of behaviour needs to be specialcased to collections.
 * none of the interfaces that exist are safe to use since
 * technically any user code could implement them and the lazyiterables
 * in hack do not follow the same rules.
 */
function hacklib_is_collection($v) {
  static $accepted_types = array(
    "HH\Pair" => true,
    "HH\Vector" => true,
    "HH\ImmVector" => true,
    "HH\Set" => true,
    "HH\ImmSet" => true,
    "HH\Map" => true,
    "HH\ImmMap" => true);
  return is_object($v) && array_key_exists(get_class($v), $accepted_types);
}

/**
 * When a hack collection is cast to array, this calls toArray
 */
function hacklib_cast_as_array($c) {
  return hacklib_is_collection($c) ? $c->toArray() : $c;
}

/**
 * When a hack collection is cast to boolean, this checks toEmpty
 */
function hacklib_cast_as_boolean($c) {
  return hacklib_is_collection($c) ? !($c->isEmpty()) : $c;
}

/**
 *  Specialcasing of equality for hack collections.
 */
function hacklib_equals($o1, $o2) {
  if (hacklib_is_collection($o1)) {
    return $o1->hacklib_equals($o2);
  }
  if (hacklib_is_collection($o2)) {
    return $o2->hacklib_equals($o1);
  }
  return $o1 == $o2;
}

/**
 *  Specialcasing of inequality for hack collections.
 */
function hacklib_not_equals($o1, $o2) {
  if ($o1 instanceof ConstCollection) {
    return !$o1->hacklib_equals($o2);
  }
  if ($o2 instanceof ConstCollection) {
    return !$o2->hacklib_equals($o1);
  }
  return $o1 != $o2;
}

/**
 * instanceof has to make adjustments for the fact that
 * arrays are Traversable
 */
function hacklib_instanceof($e, $instStr) {
  if ($instStr === 'HH\Traversable' && is_array($e)) {
    return true;
  }
  return $e instanceof $instStr;
}

final class HACKLIB_NullObject {
  private static $instance = null;
  public static function getInstance() {
    if (self::$instance) {
      return self::$instance;
    }
    return self::$instance = new HACKLIB_NullObject();
  }

  private function __construct() {}

  public function __call($method, array $arguments) {
    return null;
  }
}
