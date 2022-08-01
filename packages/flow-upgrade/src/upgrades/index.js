/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {Upgrade} from '../Types';

import v0_170_0 from './0.170.0';
import v0_176_0 from './0.176.0';

/**
 * Holds all of the upgrades that need to be run to upgrade to each version from
 * the last version of Flow.
 */
export const VERSION_UPGRADES: $ReadOnlyArray<Upgrade> = [v0_170_0, v0_176_0];
