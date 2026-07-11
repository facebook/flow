/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

export component Widget(title: string, value: number) {
  return (
    <div>
      <h3>{title}</h3>
      <span>{value.toFixed(2)}</span>
    </div>
  );
}

export component PercentWidget(title: string, ratio: number) renders Widget {
  const pct = Math.min(100, Math.max(0, ratio * 100));
  return <Widget title={title} value={pct} />;
}

export component DeltaWidget(title: string, current: number, previous: number) renders Widget {
  const value = previous === 0 ? current : current - previous;
  return <Widget title={title} value={value} />;
}

export component Dashboard(title: string, children: renders* Widget) {
  return (
    <section>
      <h2>{title}</h2>
      {children}
    </section>
  );
}

export component App() {
  return (
    <Dashboard title="System Metrics">
      <Widget title="CPU Load" value={72.5} />
      <PercentWidget title="Memory Usage" ratio={0.847} />
      <DeltaWidget title="Request Delta" current={1250} previous={980} />
    </Dashboard>
  );
}
