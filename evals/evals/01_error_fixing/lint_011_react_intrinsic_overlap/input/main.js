/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

function label(text: string): React.Node {
  return <b>{text}</b>;
}

export default component Field(caption: string) {
  return <label>{label(caption)}</label>;
}
