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
declare class Symbol {
  /**
   * Returns a new unique Symbol value.
   * @param value Description of the new Symbol object.
   */
  static (value?: unknown): symbol;
  /**
   * A method that returns the default async iterator for an object. Called by the semantics of
   * the for-await-of statement.
   */
  static readonly asyncIterator: '@@asyncIterator'; // polyfill '@@asyncIterator'
  /**
   * Returns a Symbol object from the global symbol registry matching the given key if found.
   * Otherwise, returns a new symbol with this key.
   * @param key key to search for.
   */
  static for(key: string): symbol;
  /**
   * Expose the [[Description]] internal slot of a symbol directly.
   */
  readonly description: string | void;
  /**
   * A method that determines if a constructor object recognizes an object as one of the
   * constructor's instances. Called by the semantics of the instanceof operator.
   */
  static readonly hasInstance: unique symbol;
  /**
   * A Boolean value that if true indicates that an object should flatten to its array elements
   * by Array.prototype.concat.
   */
  static readonly isConcatSpreadable: unique symbol;
  /**
   * A method that returns the default iterator for an object. Called by the semantics of the
   * for-of statement.
   */
  static readonly iterator: '@@iterator'; // polyfill '@@iterator'
  /**
   * Returns a key from the global symbol registry matching the given Symbol if found.
   * Otherwise, returns a undefined.
   * @param sym Symbol to find the key for.
   */
  static keyFor(sym: symbol): ?string;
  static readonly length: 0;
  /**
   * A regular expression method that matches the regular expression against a string. Called
   * by the String.prototype.match method.
   */
  static readonly match: unique symbol;
  /**
   * A regular expression method that matches the regular expression against a string. Called
   * by the String.prototype.matchAll method.
   */
  static readonly matchAll: unique symbol;
  /**
   * A symbol used as the key for the metadata object attached to a decorated class or class member.
   */
  static readonly metadata: unique symbol;
  /**
   * A regular expression method that replaces matched substrings of a string. Called by the
   * String.prototype.replace method.
   */
  static readonly replace: unique symbol;
  /**
   * A regular expression method that returns the index within a string that matches the
   * regular expression. Called by the String.prototype.search method.
   */
  static readonly search: unique symbol;
  /**
   * A function valued property that is the constructor function that is used to create
   * derived objects.
   */
  static readonly species: unique symbol;
  /**
   * A regular expression method that splits a string at the indices that match the regular
   * expression. Called by the String.prototype.split method.
   */
  static readonly split: unique symbol;
  /**
   * A method that converts an object to a corresponding primitive value.
   * Called by the ToPrimitive abstract operation.
   */
  static readonly toPrimitive: unique symbol;
  /**
   * A String value that is used in the creation of the default string description of an object.
   * Called by the built-in method Object.prototype.toString.
   */
  static readonly toStringTag: unique symbol;
  /**
   * An Object whose own property names are property names that are excluded from the 'with'
   * environment bindings of the associated objects.
   */
  static readonly unscopables: unique symbol;
  /**
   * A symbol that would be used to define cleanup behavior when an object is disposed.
   * Note: Flow does not currently support the `using` declaration syntax.
   */
  static readonly dispose: '@@dispose'; // polyfill '@@dispose'
  /**
   * A symbol that would be used to define cleanup behavior when an object is disposed asynchronously.
   * Note: Flow does not currently support the `using` declaration syntax.
   */
  static readonly asyncDispose: '@@asyncDispose'; // polyfill '@@asyncDispose'
  toString(): string;
  valueOf(): ?symbol;
}

// Aliases for the well-known symbols, retained so libdefs that reference these
// names keep resolving. Each maps to the corresponding `Symbol.*` member type.
type $SymbolHasInstance = typeof Symbol.hasInstance;
type $SymbolIsConcatSpreadable = typeof Symbol.isConcatSpreadable;
type $SymbolIterator = typeof Symbol.iterator;
type $SymbolMatch = typeof Symbol.match;
type $SymbolMatchAll = typeof Symbol.matchAll;
type $SymbolReplace = typeof Symbol.replace;
type $SymbolSearch = typeof Symbol.search;
type $SymbolSpecies = typeof Symbol.species;
type $SymbolSplit = typeof Symbol.split;
type $SymbolToPrimitive = typeof Symbol.toPrimitive;
type $SymbolToStringTag = typeof Symbol.toStringTag;
type $SymbolUnscopables = typeof Symbol.unscopables;
type $SymbolDispose = typeof Symbol.dispose;
type $SymbolAsyncDispose = typeof Symbol.asyncDispose;
