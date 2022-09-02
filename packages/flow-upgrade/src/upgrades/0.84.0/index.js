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

import convertImplicitInexactObjectTypes from '../../codemods/convertImplicitInexactObjectTypes';

export default ({
  version: '0.84.0',
  upgrades: [convertImplicitInexactObjectTypes],
}: Upgrade);
