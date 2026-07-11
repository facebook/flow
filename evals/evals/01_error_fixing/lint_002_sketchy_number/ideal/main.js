/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export default component CommentBadge(count: number) {
  return <span>{count > 0 ? <b>{count} comments</b> : null}</span>;
}
