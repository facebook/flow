/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// .ts consumer: Box<Dog> -> Box<Animal> uses the generic's relaxed
// covariant treatment of neutral type parameters.

import {type Animal, type Dog, Box} from "./cross_lang_lib";

declare const dogBox: Box<Dog>;
const a: Box<Animal> = dogBox; // OK in .ts
