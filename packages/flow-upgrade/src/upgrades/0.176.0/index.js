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

import removeAnnotationsInDestructuring from '../../codemods/removeAnnotationsInDestructuring';

export default ({
  version: '0.176.0',
  upgrades: [removeAnnotationsInDestructuring],
}: Upgrade);
