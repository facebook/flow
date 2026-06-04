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
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './Hero.module.css';
import buttonStyles from './Buttons.module.css';

export default function Hero(): React.Node {
  const logoUrl = useBaseUrl('/img/logomark.svg');
  return (
    <header className={styles.hero}>
      <div className={styles.container}>
        <img
          className={styles.logoMark}
          src={logoUrl}
          alt=""
          aria-hidden="true"
        />
        <h1 className={styles.title}>
          <span className={styles.titleAccent}>Flow</span> is a typed dialect of
          JavaScript.
        </h1>
        <p className={styles.subtitle}>
          It looks like TypeScript now. Plus <code>component</code> /{' '}
          <code>renders</code> / <code>match</code>. And still safer.
        </p>
        <div className={styles.ctaRow}>
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
            Coming from TypeScript?
          </Link>
          <Link
            className={`${buttonStyles.button} ${buttonStyles.buttonSecondary}`}
            to="/try">
            Try Flow in browser
          </Link>
        </div>
      </div>
    </header>
  );
}
