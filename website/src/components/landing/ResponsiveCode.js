/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import styles from './ResponsiveCode.module.css';

// Two variants of the same code block: one tuned for the wide two-column
// desktop layout, one tuned for the stacked single-column phone layout. Both
// children are ordinary ```js flow-check blocks, so both are type-checked by
// the remark plugin at build time and by the website snapshot test. CSS shows
// exactly one at a time, switching at the same width the showcase row stacks
// (max-width: 1024px in Showcase.module.css).
export function CodeDesktop({children}: {children: React.Node}): React.Node {
  return <div className={styles.desktopOnly}>{children}</div>;
}
export function CodeMobile({children}: {children: React.Node}): React.Node {
  return <div className={styles.mobileOnly}>{children}</div>;
}
