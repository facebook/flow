/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

export default function assertNoLogFileOption(flowconfig: string): void {
  if (/^log\.file/mu.test(flowconfig)) {
    throw new Error(
      'Unsupported .flowconfig option `log.file`. The VS Code extension does not support this option.',
    );
  }
}
