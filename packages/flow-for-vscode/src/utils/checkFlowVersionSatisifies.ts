/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import semver from 'semver';

export default function checkFlowVersionSatisfies(
  version: string,
  range: string,
): boolean {
  return semver.satisfies(version, range, {
    includePrerelease: true,
  });
}
