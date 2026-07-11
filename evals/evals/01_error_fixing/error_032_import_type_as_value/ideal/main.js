/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import type {User} from 'user';
import {makeUser} from 'user';

const admin: User = makeUser(1, 'Ada');
console.log(`${admin.id}: ${admin.name}`);
