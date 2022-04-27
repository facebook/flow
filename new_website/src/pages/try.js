/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

import * as React from 'react';
import useDocusaurusContext from '@docusaurus/useDocusaurusContext';
import Layout from '@theme/Layout';
import Navbar from '@theme/Navbar';
import TryFlow from '../try-flow/TryFlow';

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
      <TryFlow
        defaultFlowVersion={defaultFlowVersion}
        flowVersions={flowVersions}
      />
    </Layout>
  );
}
