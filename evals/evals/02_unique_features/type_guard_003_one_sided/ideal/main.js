/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

function isUsableReading(temp: ?number): implies temp is number {
  return temp != null && temp >= -40 && temp <= 125;
}

function format(temp: ?number): string {
  if (isUsableReading(temp)) {
    return `Reading: ${temp.toFixed(1)}C`;
  }
  return 'No usable reading';
}

const samples: Array<?number> = [21.5, null, 999, -50, 37.2];
for (const sample of samples) {
  console.log(format(sample));
}
