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
  const point = event.payload as unknown as Point;
  return Math.sqrt(point.x ** 2 + point.y ** 2);
}

export {distanceFromOrigin};
