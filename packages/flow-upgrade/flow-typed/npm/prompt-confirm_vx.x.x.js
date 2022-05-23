/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

declare module 'prompt-confirm' {
  declare class Confirm {
    constructor(
      opt:
        | string
        | $ReadOnly<{
            name: string,
            message: string,
          }>,
    ): Confirm;

    ask(cb: (?string) => void): void;
    run(): Promise<?string>;
  }
  declare module.exports: typeof Confirm;
}
