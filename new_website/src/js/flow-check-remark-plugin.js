/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const getFlowErrors = require('./initialized-flow-provider');

const transformNode = node => [
  {
    type: 'jsx',
    value: `<FlowCheckCodeBlock flowErrors={${JSON.stringify(
      getFlowErrors(node.value),
    )}}>`,
  },
  {type: 'text', value: node.value},
  {type: 'jsx', value: '</FlowCheckCodeBlock>'},
];

const matchNode = node => node.type === 'code' && node.meta === 'flow-check';

const nodeForImport = {
  type: 'import',
  value:
    "import FlowCheckCodeBlock from '@site/src/components/FlowCheckCodeBlock';",
};

module.exports = () => {
  let transformed = false;
  let alreadyImported = false;
  const transformer = node => {
    if (
      node.type === 'import' &&
      node.value.includes('@site/src/components/FlowCheckCodeBlock')
    ) {
      alreadyImported = true;
    }
    if (matchNode(node)) {
      transformed = true;
      return transformNode(node);
    }
    if (Array.isArray(node.children)) {
      let index = 0;
      while (index < node.children.length) {
        const result = transformer(node.children[index]);
        if (result) {
          node.children.splice(index, 1, ...result);
          index += result.length;
        } else {
          index += 1;
        }
      }
    }
    if (node.type === 'root' && transformed && !alreadyImported) {
      node.children.unshift(nodeForImport);
    }
    return null;
  };
  return transformer;
};
