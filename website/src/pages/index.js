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
import Layout from '@theme/Layout';
import Link from '@docusaurus/Link';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import useBaseUrl from '@docusaurus/useBaseUrl';
import styles from './styles.module.css';

// This import serves no runtime purposes, but we import it to force webpack to run babel on it,
// so we can test whether babel can handle newer syntax.
import '../js/parser-playground';

export default component Home() {
  const context = useDocusaurusContext();
  const {siteConfig} = context;
  return (
    <Layout
      title="Flow: A Static Type Checker for JavaScript"
      description={siteConfig.description}>
      <header className={clsx(styles.feature, styles.featureHero)}>
        <div className="container">
          <p className={styles.featureHeading}>
            <span>Flow is</span> <br className={styles.hiddenLargerUp} /> a
            static type <br className={styles.hiddenLargerUp} /> checker for{' '}
            <br className={styles.hiddenLargerUp} /> <span>JavaScript.</span>
          </p>
          <Link
            className={styles.featureButton}
            to={useBaseUrl('en/docs/getting-started')}>
            Get Started
          </Link>
          <Link
            className={styles.featureButton}
            to={useBaseUrl('en/docs/install')}>
            Install Flow
          </Link>
          <iframe
            className="gh-btn"
            src="https://ghbtns.com/github-btn.html?user=facebook&repo=flow&type=star&count=false&size=large"
            frameBorder="0"
            scrolling="0"
            width="160px"
            height="30px"
            title="Star"></iframe>
          <div className={clsx(styles.featureHeroText)}>
            Current version:{' '}
            <strong className="navbar-text">
              {/* TODO: add release */}
              <a
                href={`https://github.com/facebook/flow/releases/tag/${siteConfig.customFields.flowVersion}`}
                className={styles.releaseVersion}>
                {siteConfig.customFields.flowVersion}
              </a>
            </strong>
          </div>
        </div>
      </header>
      <main>
        <section className={clsx(styles.feature, styles.featureLight)}>
          <div
            className={clsx(
              styles.featureDecoration,
              styles.featureDecorationDrop,
            )}
          />
          <div className="container">
            <div className="row">
              <div className="col col--7">
                <h2
                  className={clsx(
                    styles.featureHeading,
                    styles.featureHeadingCenter,
                  )}>
                  Code Faster.
                </h2>
                <p className={styles.featureText}>
                  Tired of having to run your code to find bugs? Flow identifies
                  problems as you code. Stop wasting your time guessing and
                  checking.
                </p>
              </div>
              <div className="col col--5">
                <img
                  className={clsx(styles.imgFluid, 'rounded')}
                  width={890}
                  height={564}
                  src={useBaseUrl('img/featurette-faster.gif')}
                />
              </div>
            </div>
          </div>
        </section>

        <section className={clsx(styles.feature, styles.featureGray)}>
          <div className="container">
            <div className="row">
              <div className="col col--5">
                <img
                  className={clsx(styles.imgFluid, 'rounded')}
                  width={890}
                  height={564}
                  src={useBaseUrl('img/featurette-smarter.gif')}
                />
              </div>
              <div className="col col--7">
                <h2
                  className={clsx(
                    styles.featureHeading,
                    styles.featureHeadingCenter,
                  )}>
                  Code Smarter.
                </h2>
                <p className={styles.featureText}>
                  It's hard to build smart tools for dynamic languages like
                  JavaScript. Flow understands your code and makes its knowledge
                  available, enabling other smart tools to be built on top of
                  Flow.
                </p>
              </div>
            </div>
          </div>
        </section>

        <section className={clsx(styles.feature, styles.featureDark)}>
          <div
            className={clsx(
              styles.featureDecoration,
              styles.featureDecorationDrop,
            )}
          />
          <div
            className={clsx(
              styles.featureDecoration,
              styles.featureDecorationRise,
            )}
          />
          <div className="container">
            <div className="row">
              <div className="col col--7">
                <h2
                  className={clsx(
                    styles.featureHeading,
                    styles.featureHeadingCenter,
                  )}>
                  Code Confidently.
                </h2>
                <p className={styles.featureText}>
                  Making major changes to large codebases can be scary. Flow
                  helps you refactor safely, so you can focus on the changes you
                  want to make, and stop worrying about what you might break.
                </p>
              </div>
              <div className="col col--5">
                <img
                  className={clsx(styles.imgFluid, 'rounded')}
                  width={890}
                  height={564}
                  src={useBaseUrl('img/featurette-confidently.gif')}
                />
              </div>
            </div>
          </div>
        </section>

        <section className={clsx(styles.feature, styles.featureLight)}>
          <div className="container">
            <div className="row">
              <div className="col col--5">
                <img
                  className={clsx(styles.imgFluid, 'rounded')}
                  width={890}
                  height={564}
                  src={useBaseUrl('img/featurette-bigger.gif')}
                />
              </div>
              <div className="col col--7">
                <h2
                  className={clsx(
                    styles.featureHeading,
                    styles.featureHeadingCenter,
                  )}>
                  Code Bigger.
                </h2>
                <p className={styles.featureText}>
                  Working in a codebase with lots of developers can make it
                  difficult to keep your master branch working. Flow can help
                  prevent bad rebases. Flow can help protect your carefully
                  designed library from misuse and misinterpretation. And Flow
                  can help you understand the code you wrote six months ago.
                </p>
              </div>
            </div>
          </div>
        </section>

        <section
          className={clsx(
            styles.feature,
            styles.featureYellow,
            styles.featureSmall,
          )}>
          <div
            className={clsx(
              styles.featureDecoration,
              styles.featureDecorationRise,
            )}
          />
          <div
            className={clsx(
              styles.featureDecoration,
              styles.featureDecorationDrop,
            )}
          />
          <div className="container">
            <h2 className={styles.featureHeading}>Ready to get going?</h2>
            <div>
              <Link
                className={styles.featureButton}
                to={useBaseUrl('en/docs/getting-started')}>
                Get Started
              </Link>
              <Link
                className={styles.featureButton}
                to={useBaseUrl('en/docs/install')}>
                Install Flow
              </Link>
            </div>
          </div>
        </section>

        <section
          className={clsx(
            styles.feature,
            styles.featureLight,
            styles.featureSmall,
          )}>
          <div className="container">
            <div className="row">
              <div className={clsx('col col--6', styles.featurette)}>
                <h2 className={styles.featuretteHeading}>Realtime Feedback</h2>
                <p className={styles.featuretteText}>
                  Flow gives you fast feedback while you code by incrementally
                  rechecking your code as you make changes.
                </p>
              </div>
              <div className={clsx('col col--6', styles.featurette)}>
                <h2 className={styles.featuretteHeading}>Easy Integration</h2>
                <p className={styles.featuretteText}>
                  Flow integrates well with many tools, making it easy to insert
                  into your existing workflow and toolchain.
                </p>
              </div>
            </div>
          </div>
        </section>
      </main>
    </Layout>
  );
}
