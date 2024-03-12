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

export default component TryFlowPage() {
  const context = useDocusaurusContext();
  const {siteConfig = ({}: any)} = context;
  return (
    <Layout
      // $FlowFixMe[prop-missing]
      title="Try Flow: the Flow Playground"
      // $FlowFixMe[prop-missing]
      description={siteConfig.description}
      noFooter>
      <BrowserOnly>
        {() => (
          <React.Suspense fallback={<div>Loading...</div>}>
            <TryFlow
              defaultFlowVersion={siteConfig.customFields.flowVersion}
              flowVersions={siteConfig.customFields.allFlowVersions}
            />
          </React.Suspense>
        )}
      </BrowserOnly>
    </Layout>
  );
}
