/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import clsx from 'clsx';
import {themes as prismThemes} from 'prism-react-renderer';
import styles from './Showcase.module.css';
import Reveal from './Reveal';
import {CodeThemeContext} from './CodeThemeContext';
import {CodeErrorPlacementContext} from './CodeErrorPlacementContext';

// Slot markers. They render their children directly; Showcase pulls each
// slot's content out by matching on the component type, so the prose can sit
// with the heading in the text column while the code stands alone as the
// visual column.
export function ShowcaseText({children}: {children: React.Node}): React.Node {
  return children;
}
export function ShowcaseCode({children}: {children: React.Node}): React.Node {
  return children;
}

type Props = $ReadOnly<{
  eyebrow?: string,
  heading: React.Node,
  align?: 'left' | 'right',
  children: React.Node,
}>;

export default function Showcase({
  eyebrow,
  heading,
  align = 'left',
  children,
}: Props): React.Node {
  let textContent = null;
  let codeContent = null;
  React.Children.forEach(children, (child: any) => {
    if (child == null || typeof child !== 'object') {
      return;
    }
    if (child.type === ShowcaseText) {
      textContent = child.props.children;
    } else if (child.type === ShowcaseCode) {
      codeContent = child.props.children;
    }
  });

  return (
    <section className={styles.section}>
      <div className={styles.container}>
        <div
          className={clsx(styles.row, align === 'right' && styles.rowReverse)}>
          <Reveal className={styles.textCol}>
            {eyebrow != null && <div className={styles.eyebrow}>{eyebrow}</div>}
            <h2 className={styles.heading}>{heading}</h2>
            <div className={styles.body}>{textContent}</div>
          </Reveal>
          <Reveal className={styles.codeCol} delay={120}>
            <CodeThemeContext.Provider value={prismThemes.vsDark}>
              <CodeErrorPlacementContext.Provider value="end">
                {codeContent}
              </CodeErrorPlacementContext.Provider>
            </CodeThemeContext.Provider>
          </Reveal>
        </div>
      </div>
    </section>
  );
}
