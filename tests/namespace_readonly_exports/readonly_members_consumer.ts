/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import { ReadonlyClass, ReadonlyFunction } from './readonly_members';
import { Writable } from './readonly_members_local';

ReadonlyClass.value = 1; // ERROR
ReadonlyClass.alias = 1; // ERROR
ReadonlyClass.mutable = 1;
ReadonlyClass.variable = 1;
ReadonlyClass.mutableAlias = 1;

ReadonlyFunction.value = 1; // ERROR
ReadonlyFunction.alias = 1; // ERROR
ReadonlyFunction.mutable = 1;
ReadonlyFunction.variable = 1;
ReadonlyFunction.mutableAlias = 1;

Writable.value = 3;
