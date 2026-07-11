/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

type CardProps = {
  title: string,
  body: string,
  elevated?: boolean,
};

function Card(props: CardProps): React.Node {
  return (
    <div className={props.elevated === true ? 'card elevated' : 'card'}>
      <h3>{props.title}</h3>
      <p>{props.body}</p>
    </div>
  );
}

function ElevatedCard(props: CardProps): React.Node {
  return <Card {...props} elevated={true} />;
}

export default ElevatedCard;
