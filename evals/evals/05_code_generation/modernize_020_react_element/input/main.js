/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

component Header(title: string) {
  return <h1>{title}</h1>;
}

component Page(header: React.Element<typeof Header>, body: React.Node) {
  return (
    <div>
      {header}
      <main>{body}</main>
    </div>
  );
}

export component App() {
  return <Page header={<Header title="Welcome" />} body={<p>Hello, world</p>} />;
}
