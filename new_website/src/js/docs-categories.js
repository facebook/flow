/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {docsSidebar} = require('../../sidebars.js');

const docsCategories: Array<{
  docId: string,
  href: string,
  label: string,
  type: string,
}> = docsSidebar.map(item => {
  const label = item.label;
  const id = item.type === 'doc' ? item.id : item.items[0];
  let href = `/en/docs/${id}`;
  if (href.endsWith('/index')) href = href.substring(0, href.length - 6);
  return {type: 'link', docId: id, href, label};
});

export default docsCategories;
