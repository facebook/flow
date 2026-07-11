/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

function defineAction<const T>(type: T): { type: T } {
  return { type };
}

const ADD = defineAction("add");
const REMOVE = defineAction("remove");

const added: "add" = ADD.type;
const removed: "remove" = REMOVE.type;

function apply(action: { type: "add" } | { type: "remove" }): number {
  return action.type === "add" ? 1 : -1;
}

console.log(added, removed, apply(ADD), apply(REMOVE));
