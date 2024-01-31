/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

import type {Upgrade} from '../../Types';

import renamePartial from '../../codemods/renamePartial';

export default {
  version: '0.201.0',
  upgrades: [renamePartial],
} as Upgrade;
