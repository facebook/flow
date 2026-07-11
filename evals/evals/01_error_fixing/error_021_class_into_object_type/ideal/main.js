/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

class SidebarWidget {
  id: string;
  size: number;
  constructor(id: string, size: number) {
    this.id = id;
    this.size = size;
  }
}

function renderNode(node: interface {id: string, size: number}): string {
  const width = Math.max(node.size * 2, node.id.length + 2);
  const bar = '='.repeat(width);
  return bar + '\n[' + node.id + '] size=' + String(node.size) + '\n' + bar;
}

const widget = new SidebarWidget('nav', 6);
const rendered = renderNode(widget);
console.log(rendered);
