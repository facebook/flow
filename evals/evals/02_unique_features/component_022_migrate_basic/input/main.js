/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

type Props = {
  title: string,
  count: number,
  subtitle?: string,
};

function StatCard(props: Props): React.Node {
  return (
    <div>
      <h3>{props.title}</h3>
      {props.subtitle != null ? <p>{props.subtitle}</p> : null}
      <span>{props.count}</span>
    </div>
  );
}

export default StatCard;
