/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 * @format
 */

'use strict';

import type {DocblockMetadata} from 'flow-estree';

const FLOW_DIRECTIVE = /(@flow(\s+(strict(-local)?|weak))?|@noflow)/;

export function removeAtFlowFromDocblock(
  docblock: DocblockMetadata,
): DocblockMetadata {
  if (!FLOW_DIRECTIVE.test(docblock.comment.value)) {
    return docblock;
  }

  return {
    // $FlowExpectedError[cannot-spread-interface]
    comment: {
      ...docblock.comment,
      value: docblock.comment.value.replace(FLOW_DIRECTIVE, ''),
    },
    directives: {
      ...docblock.directives,
      flow: undefined,
      noflow: undefined,
    },
  };
}
