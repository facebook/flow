/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import binVersion from 'bin-version';

export default function getFlowVersion(flowPath: string): Promise<string> {
  return binVersion(flowPath);
}
