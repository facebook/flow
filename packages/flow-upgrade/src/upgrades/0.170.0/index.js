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

import removeDuplicateClassProperties from '../../codemods/removeDuplicateClassProperties';

export default ({
  version: '0.170.0',
  upgrades: [removeDuplicateClassProperties],
}: Upgrade);
