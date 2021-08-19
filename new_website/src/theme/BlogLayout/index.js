/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import React from 'react';
import clsx from 'clsx';
import Layout from '@theme/Layout';
import BlogSidebar from '@theme/BlogSidebar';
import TOC from '@theme/TOC';

function BlogLayout(props) {
  const {toc, children, ...layoutProps} = props;
  return (
    <Layout {...layoutProps}>
      <div className="container margin-vert--lg">
        <div className="row">
          <main className="col col--9 col--offset-1">
            {children}
          </main>
          {toc && (
            <div className="col col--2">
              <TOC toc={toc} />
            </div>
          )}
        </div>
      </div>
    </Layout>
  );
}

export default BlogLayout;
