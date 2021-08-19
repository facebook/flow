/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import flow from './flow/flow';
import CoreFlowLib from '!!raw-loader!./flow/lib/core';
import ReactFlowLib from '!!raw-loader!./flow/lib/react';
import IntlFlowLib from '!!raw-loader!./flow/lib/intl';

const TRY_LIB_CONTENTS = `
declare type $JSXIntrinsics = {
  [string]: {
    instance: any,
    props: {
      children?: React$Node,
      [key: string]: any,
    },
  },
};
`.slice(1);

const libs = [
  ['/core.js', CoreFlowLib],
  ['/react.js', ReactFlowLib],
  ['/intl.js', IntlFlowLib],
  ['jsx-lib.js', TRY_LIB_CONTENTS],
];

libs.forEach(([name, content]) => flow.registerFile(name, content));
flow.initBuiltins(libs.map(([name]) => name));

export default flow;
