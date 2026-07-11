/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Transform = {
  id: string,
  rotation?: Array<number>,
  scale?: Array<number>,
};

function buildTransform(item: Transform): Transform {
  const rotation: {rotation?: Array<number>} = item.rotation
    ? {rotation: [...item.rotation]}
    : {};
  const scale: {scale?: Array<number>} = item.scale
    ? {scale: [...item.scale]}
    : {};
  return {id: item.id, ...rotation, ...scale};
}

console.log(buildTransform({id: 'node', rotation: [0, 90, 0]}));
