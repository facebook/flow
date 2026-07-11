/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

component Card(
  title: string,
  body: React.Node,
  variant: 'default' | 'highlight' = 'default',
) {
  const className = variant === 'highlight' ? 'card card-highlight' : 'card';
  return (
    <section className={className}>
      <h3>{title}</h3>
      <div className="card-body">{body}</div>
    </section>
  );
}

function renderWithOverrides(
  base: React.ElementConfig<typeof Card>,
  overrides: Partial<React.ElementConfig<typeof Card>>,
): React.Node {
  return <Card {...base} {...overrides} />;
}

export default component App() {
  const base = {
    title: 'Welcome',
    body: <p>Getting started is easy.</p>,
  };
  return (
    <div>
      {renderWithOverrides(base, {})}
      {renderWithOverrides(base, {variant: 'highlight'})}
    </div>
  );
}
