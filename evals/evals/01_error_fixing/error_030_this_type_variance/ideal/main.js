/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

class ListNode {
  value: number;
  prev: ListNode | null;

  constructor(value: number) {
    this.value = value;
    this.prev = null;
  }

  append(value: number): ListNode {
    const node = new ListNode(value);
    node.prev = this;
    return node;
  }
}

const head = new ListNode(1);
const second = head.append(2);
const third = second.append(3);
console.log(third.prev?.value);
