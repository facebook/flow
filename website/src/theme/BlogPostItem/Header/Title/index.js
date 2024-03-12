/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

import * as React from 'react';
import clsx from 'clsx';
import Link from '@docusaurus/Link';
import {useBlogPost} from '@docusaurus/theme-common/internal';

import styles from './styles.module.css';

export default component BlogPostItemHeaderTitle(className: string) {
  const {metadata} = useBlogPost();
  const {permalink, title, frontMatter} = metadata;
  const mediumLink = frontMatter['medium-link'];
  const isBlogPostPage = mediumLink == null;
  const TitleHeading = isBlogPostPage ? 'h1' : 'h2';
  return (
    <TitleHeading className={clsx(styles.title, className)} itemProp="headline">
      {isBlogPostPage ? (
        title
      ) : (
        <Link itemProp="url" to={mediumLink}>
          {title}
        </Link>
      )}
    </TitleHeading>
  );
}
