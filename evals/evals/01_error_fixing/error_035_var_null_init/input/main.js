/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function summarize(labels: Array<string>): string | number {
  let summary = null;
  for (const label of labels) {
    summary = label;
  }
  if (labels.length === 0) {
    summary = 0;
  }
  return summary ?? 'none';
}

summarize(['a', 'b']);
