/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

import * as React from 'react';

type PanelPlugin = component(title: string, content: string);

function renderPlugins(
  plugins: ReadonlyArray<PanelPlugin>,
  data: ReadonlyArray<{title: string, content: string}>,
): React.Node {
  return data.map((entry, i) => {
    const Plugin = plugins[i % plugins.length];
    return <Plugin key={i} title={entry.title} content={entry.content} />;
  });
}

export component CardPanel(title: string, content: string) {
  return (
    <div>
      <h3>{title}</h3>
      <p>{content}</p>
    </div>
  );
}

export component MinimalPanel(title: string, content: string) {
  return (
    <span>{title}: {content}</span>
  );
}

export component App() {
  const plugins: ReadonlyArray<PanelPlugin> = [CardPanel, MinimalPanel];
  const data = [
    {title: 'Status', content: 'All systems operational'},
    {title: 'Users', content: '1,234 active'},
    {title: 'Latency', content: '45ms p99'},
  ];
  return <div>{renderPlugins(plugins, data)}</div>;
}
