/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict-local
 */

// A plain toast notification target.
type Toast = {message: string};

// A console target that also displays numeric status codes.
type ConsoleTarget = {message: string | number};

function notify(target: {writeonly message: string}, text: string): void {
  target.message = text;
}

const toast: Toast = {message: 'init'};
const consoleTarget: ConsoleTarget = {message: 0};

notify(toast, 'saved');
notify(consoleTarget, 'updated');
