/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export component Header(text: string) {
  return <h1>{text}</h1>;
}

export component SectionHeader(section: string) renders Header {
  return <Header text={section} />;
}

export component Layout(header: renders Header, body: string) {
  return (
    <div>
      {header}
      <main>{body}</main>
    </div>
  );
}

export component App() {
  return <Layout header={<SectionHeader section="Dashboard" />} body="Welcome back" />;
}
