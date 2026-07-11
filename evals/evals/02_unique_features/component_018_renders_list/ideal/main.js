/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export component Tab(label: string) {
  return <li>{label}</li>;
}

export component TabBar(children: renders* Tab) {
  return <ul>{children}</ul>;
}

export component CountTab(label: string, count: number) renders Tab {
  return <Tab label={label + ' (' + count + ')'} />;
}

export component App() {
  return (
    <TabBar>
      <Tab label="Home" />
      <CountTab label="Inbox" count={5} />
      {[<Tab key="a" label="Archive" />, <Tab key="b" label="Spam" />]}
    </TabBar>
  );
}
