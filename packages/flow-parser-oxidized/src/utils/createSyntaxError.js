/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

import type {ESNode} from 'flow-estree-oxidized';

export function createSyntaxError(node: ESNode, err: string): SyntaxError {
  const syntaxError = new SyntaxError(err);
  // $FlowExpectedError[prop-missing]
  syntaxError.loc = {
    line: node.loc.start.line,
    column: node.loc.start.column,
  };

  return syntaxError;
}
