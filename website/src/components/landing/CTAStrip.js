/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import Link from '@docusaurus/Link';
import styles from './CTAStrip.module.css';
import buttonStyles from './Buttons.module.css';

export default function CTAStrip(): React.Node {
  return (
    <section className={styles.strip}>
      <div className={styles.container}>
        <div className={styles.rule} aria-hidden="true" />
        <h2 className={styles.heading}>Use Flow in your project</h2>
        <div className={styles.ctas}>
          <Link
            className={`${buttonStyles.button} ${styles.buttonPrimary}`}
            to="/en/docs/getting-started">
            Get started{' '}
            <span className={buttonStyles.arrow} aria-hidden="true">
              →
            </span>
          </Link>
          <Link
            className={`${buttonStyles.button} ${buttonStyles.buttonSecondary}`}
            to="/en/docs/flow-vs-typescript">
            Flow for TypeScript users in 2026
          </Link>
          <Link
            className={`${buttonStyles.button} ${buttonStyles.buttonSecondary}`}
            to="/try">
            Try Flow in browser
          </Link>
        </div>
        <p className={styles.footnote}>
          Used in production at Meta across millions of files of JavaScript and
          React.
        </p>
      </div>
    </section>
  );
}
