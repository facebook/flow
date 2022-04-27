/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import OriginalCodeBlock from '@theme-original/CodeBlock';
import FlowCheckCodeBlock from '../../components/FlowCheckCodeBlock';

export default function CodeBlock(props: any): React.MixedElement {
  return props.className === 'language-flow' ? (
    <FlowCheckCodeBlock {...props} />
  ) : (
    <OriginalCodeBlock {...props} />
  );
}
