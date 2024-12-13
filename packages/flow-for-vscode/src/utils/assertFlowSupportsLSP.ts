/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import checkFlowVersionSatisfies from './checkFlowVersionSatisifies';

const FLOW_VERSION_FOR_LSP = '>=0.75';

export default function assertFlowSupportsLSP(version: string): void {
  if (!checkFlowVersionSatisfies(version, FLOW_VERSION_FOR_LSP)) {
    throw new Error(
      `Flow version ${version} doesn't support 'flow lsp'.` +
        ` Please upgrade flow to version ${FLOW_VERSION_FOR_LSP}.`,
    );
  }
}
