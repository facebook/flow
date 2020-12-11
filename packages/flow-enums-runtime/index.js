/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

'use strict';

// Below we want to use `hasOwnProperty` on an object that doesn't have
// `Object.prototype` in its proto chain, so we must extract it here.
var hasOwnProperty = Object.prototype.hasOwnProperty;

// Map from an enum object to a set of its values.
var enumValuesCache = typeof WeakMap === 'function' ? new WeakMap() : new Map();

function valuesSet(enumObject) {
  var enumValues = enumValuesCache.get(enumObject);
  if (enumValues !== undefined) {
    return enumValues;
  }
  // We aren't using `Object.values` because that gets enumerable
  // properties, and our properties aren't enumerable.
  var names = Object.getOwnPropertyNames(enumObject);
  var newEnumValues = new Set(names.map(name => enumObject[name]));
  enumValuesCache.set(enumObject, newEnumValues);
  return newEnumValues;
}

var EnumPrototype = Object.freeze(
  Object.defineProperties(Object.create(null), {
    isValid: {
      value: function (x) {
        return valuesSet(this).has(x);
      },
    },
    cast: {
      value: function (x) {
        return this.isValid(x) ? x : undefined;
      },
    },
    members: {
      value: function () {
        return valuesSet(this).values();
      },
    },
  })
);

// `members` is an object mapping name to value.
function Enum(members) {
  var o = Object.create(EnumPrototype);
  for (var k in members) {
    if (hasOwnProperty.call(members, k)) {
      // Create non-enumerable properties.
      Object.defineProperty(o, k, {value: members[k]});
    }
  }
  return Object.freeze(o);
}

// Mirrored enum (string enum with no member initializers).
// Optimized implementation, taking advantage of the fact that
// keys and values are identical.
var EnumMirroredPrototype = Object.freeze(
  Object.defineProperties(Object.create(null), {
    isValid: {
      value: function (x) {
        if (typeof x === 'string') {
          return hasOwnProperty.call(this, x);
        }
        return false;
      },
    },
    cast: {
      value: EnumPrototype.cast,
    },
    members: {
      value: function () {
        // We aren't using `Object.values` because that gets enumerable
        // properties, and our properties aren't enumerable.
        return Object.getOwnPropertyNames(this);
      },
    },
  })
);

// `members` is an array of names (which, are also the values).
Enum.Mirrored = function EnumMirrored(members) {
  var o = Object.create(EnumMirroredPrototype);
  for (var i = 0, len = members.length; i < len; ++i) {
    // Value is same as key. Also, non-enumerable.
    Object.defineProperty(o, members[i], {value: members[i]});
  }
  return Object.freeze(o);
};

Object.freeze(Enum.Mirrored);

module.exports = Object.freeze(Enum);
