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

import type {SourceLocation} from 'flow-estree-oxidized';

/**
 * Utility for quickly creating source locations inline.
 */
export function loc(
  startLine: number,
  startColumn: number,
  endLine: number,
  endColumn: number,
): SourceLocation {
  return {
    start: {
      line: startLine,
      column: startColumn,
    },
    end: {
      line: endLine,
      column: endColumn,
    },
  };
}
