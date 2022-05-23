/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import BrowserOnly from '@docusaurus/BrowserOnly';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Navbar from '@theme/Navbar';

const TryFlow = React.lazy(() => import('../try-flow/TryFlow'));

// TODO: read from process.env as build time constants.
const defaultFlowVersion = 'master';
const flowVersions = ['master', 'v0.176.2'];

export default function TryFlowPage(): React.MixedElement {
  const context = useDocusaurusContext();
  const {siteConfig = {}} = context;
  return (
    <Layout
      // $FlowFixMe[prop-missing]
      title={siteConfig.title}
      // $FlowFixMe[prop-missing]
      description={siteConfig.description}
      noFooter>
      <BrowserOnly>
        {() => (
          <React.Suspense fallback={<div>Loading...</div>}>
            <TryFlow
              defaultFlowVersion={defaultFlowVersion}
              flowVersions={flowVersions}
            />
          </React.Suspense>
        )}
      </BrowserOnly>
    </Layout>
  );
}
