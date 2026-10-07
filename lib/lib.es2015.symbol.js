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
  readonly prototype: Symbol;
  new(): Symbol;
  /**
   * Returns a new unique Symbol value.
   * @param value Description of the new Symbol object.
   */
  (value?: unknown): symbol;
  /**
   * Returns a Symbol object from the global symbol registry matching the given key if found.
   * Otherwise, returns a new symbol with this key.
   * @param key key to search for.
   */
  for(key: string): symbol;
  /**
   * Returns a key from the global symbol registry matching the given Symbol if found.
   * Otherwise, returns a undefined.
   * @param sym Symbol to find the key for.
   */
  keyFor(sym: symbol): ?string;
  readonly length: 0;
}

declare var Symbol: SymbolConstructor;

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
