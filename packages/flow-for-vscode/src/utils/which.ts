/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import _which from 'which';

export default async function which(command: string): Promise<null | string> {
  try {
    return await _which(command, { pathExt: '.cmd' });
  } catch (_err) {
    return null;
  }
}
