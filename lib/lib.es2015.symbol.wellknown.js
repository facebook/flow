/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Modifications Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the
 * License at http://www.apache.org/licenses/LICENSE-2.0
 * THIS CODE IS PROVIDED ON AN *AS IS* BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, EITHER EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION ANY IMPLIED
 * WARRANTIES OR CONDITIONS OF TITLE, FITNESS FOR A PARTICULAR PURPOSE,
 * MERCHANTABILITY OR NON-INFRINGEMENT.
 * See the Apache Version 2.0 License for specific language governing permissions
 * and limitations under the License.
 *
 * @flow
 */
// @lint-ignore-every LICENSELINT
interface SymbolConstructor {
  /**
   * A method that determines if a constructor object recognizes an object as one of the
   * constructor's instances. Called by the semantics of the instanceof operator.
   */
  readonly hasInstance: unique symbol;
  /**
   * A Boolean value that if true indicates that an object should flatten to its array elements
   * by Array.prototype.concat.
   */
  readonly isConcatSpreadable: unique symbol;
  /**
   * A regular expression method that matches the regular expression against a string. Called
   * by the String.prototype.match method.
   */
  readonly match: unique symbol;
  /**
   * A regular expression method that replaces matched substrings of a string. Called by the
   * String.prototype.replace method.
   */
  readonly replace: unique symbol;
  /**
   * A regular expression method that returns the index within a string that matches the
   * regular expression. Called by the String.prototype.search method.
   */
  readonly search: unique symbol;
  /**
   * A function valued property that is the constructor function that is used to create
   * derived objects.
   */
  readonly species: unique symbol;
  /**
   * A regular expression method that splits a string at the indices that match the regular
   * expression. Called by the String.prototype.split method.
   */
  readonly split: unique symbol;
  /**
   * A method that converts an object to a corresponding primitive value.
   * Called by the ToPrimitive abstract operation.
   */
  readonly toPrimitive: unique symbol;
  /**
   * A String value that is used in the creation of the default string description of an object.
   * Called by the built-in method Object.prototype.toString.
   */
  readonly toStringTag: unique symbol;
  /**
   * An Object whose own property names are property names that are excluded from the 'with'
   * environment bindings of the associated objects.
   */
  readonly unscopables: unique symbol;
}

interface Date {
    [Symbol.toPrimitive]: (hint: 'string' | 'default' | 'number') => string | number;
}

interface ReadonlyMap<K, out V> {
    readonly [Symbol.toStringTag]: 'Map';
}

interface ReadonlySet<out T> {
    readonly [Symbol.toStringTag]: 'Set';
}

interface RegExp {
    readonly [Symbol.match]: (str: string) => RegExpStringIterator<RegExpMatchArray>;
}

interface MapConstructor {
    readonly [Symbol.species]: any;
}

interface SetConstructor {
    readonly [Symbol.species]: (...any) => any; // This would the Set constructor, can't think of a way to correctly type this
}

interface ArrayBufferConstructor {
    readonly [Symbol.species]: ArrayBufferConstructor;
}
