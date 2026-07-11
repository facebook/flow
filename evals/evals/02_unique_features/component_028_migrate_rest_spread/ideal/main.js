/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

component Card(title: string, body: string, elevated?: boolean) {
  return (
    <div className={elevated === true ? 'card elevated' : 'card'}>
      <h3>{title}</h3>
      <p>{body}</p>
    </div>
  );
}

export default component ElevatedCard(...props: React.PropsOf<Card>) {
  return <Card {...props} elevated={true} />;
}
