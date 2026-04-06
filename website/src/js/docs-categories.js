/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

type SidebarDoc = {
  type: 'doc',
  id: string,
  label: string,
};

type SidebarCategory = {
  type: 'category',
  label: string,
  items: Array<SidebarItem>,
  collapsed?: boolean,
};

export type SidebarItem = string | SidebarDoc | SidebarCategory;

const {
  docsSidebar,
}: {docsSidebar: Array<SidebarItem>} = require('../../sidebars.js');

function getFirstDocId(item: SidebarItem): string {
  if (typeof item === 'string') return item;
  if (item.type === 'doc') return item.id;
  if (item.type === 'category' && item.items.length > 0)
    return getFirstDocId(item.items[0]);
  return '';
}

function getLabel(item: SidebarItem): string {
  if (typeof item === 'string') return item;
  return item.label;
}

const docsCategories: Array<{
  docId: string,
  href: string,
  label: string,
  type: 'link',
}> = docsSidebar.map(item => {
  const label = getLabel(item);
  const id = getFirstDocId(item);
  let href = `/en/docs/${id}`;
  if (href.endsWith('/index')) href = href.substring(0, href.length - 6);
  return {type: 'link', docId: id, href, label};
});

export default docsCategories;
