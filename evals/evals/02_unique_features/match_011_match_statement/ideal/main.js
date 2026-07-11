/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Command =
  | {cmd: 'log', msg: string}
  | {cmd: 'set', key: string, value: string}
  | {cmd: 'delete', key: string};

export function execute(store: Map<string, string>, command: Command): void {
  match (command) {
    {cmd: 'log', const msg} => {
      console.log(msg);
    }
    {cmd: 'set', const key, const value} => {
      store.set(key, value);
    }
    {cmd: 'delete', const key} => {
      store.delete(key);
    }
  }
}
