/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import React from 'react';
import OriginalCodeBlock from '@theme-init/CodeBlock';
import FlowCheckCodeBlock from '../../components/FlowCheckCodeBlock';

export default function CodeBlock(props) {
  return props.className === 'language-flow' ? (
    <FlowCheckCodeBlock {...props} />
  ) : (
    <OriginalCodeBlock {...props} />
  );
}
