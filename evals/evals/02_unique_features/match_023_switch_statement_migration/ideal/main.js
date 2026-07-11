/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Command = 'undo' | 'redo' | 'clear' | 'push';

export function applyCommand(
  history: Array<string>,
  command: Command,
  label: string,
): void {
  match (command) {
    'undo' | 'redo' => {
      history.pop();
    }
    'clear' => {
      history.length = 0;
    }
    'push' => {
      history.push(label);
    }
  }
}
