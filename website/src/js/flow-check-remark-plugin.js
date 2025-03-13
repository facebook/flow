/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import getFlowErrors from './initialized-flow-provider.js';

/*:: type UnwrapPromise<T> = T extends Promise<infer V> ? V : T */

const transformNode = async (node /*: any */) => [
  {
    type: 'code',
    lang: 'flow',
    meta: await getFlowErrors(node.value),
    value: node.value,
  },
];

const matchNode = (node /*: any */) =>
  node.type === 'code' && node.meta === 'flow-check';

export default () /*: any */ => {
  let transformed = false;
  let alreadyImported = false;
  const transformer = async (
    node /*: any */,
  ) /*: Promise<UnwrapPromise<ReturnType<typeof transformNode>> | null> */ => {
    if (matchNode(node)) {
      transformed = true;
      return transformNode(node);
    }
    if (Array.isArray(node.children)) {
      let index = 0;
      while (index < node.children.length) {
        const result = await transformer(node.children[index]);
        if (result) {
          node.children.splice(index, 1, ...result);
          index += result.length;
        } else {
          index += 1;
        }
      }
    }
    return null;
  };
  return transformer;
};
