/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

type ServerEvent = {channel: string, payload: unknown};
type Point = {x: number, y: number};

function distanceFromOrigin(event: ServerEvent): number {
  const payload = event.payload;
  if (
    typeof payload === 'object' &&
    payload != null &&
    typeof payload.x === 'number' &&
    typeof payload.y === 'number'
  ) {
    return Math.sqrt(payload.x ** 2 + payload.y ** 2);
  }
  return 0;
}

export {distanceFromOrigin};
