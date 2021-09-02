/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

import React from 'react';

export function SinceVersion({version}) {
  return (
    <span class="version added" title={`Added in ${version}`}>
      &ge;{version}
    </span>
  );
}

export function UntilVersion({version}) {
  return (
    <span class="version removed" title={`Removed after ${version}`}>
      &le;{version}
    </span>
  );
}
