/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

// adapted from https://github.com/sindresorhus/ora/blob/c2400db7a3695c2c420327680aeecda98cefc289/index.d.ts

declare module 'ora' {
  declare export type Options = $ReadOnly<{
    /**
    Text to display after the spinner.
    */
    +text?: string,

    /**
    The color of the spinner.
    @default 'cyan'
    */
    +color?:
      | 'black'
      | 'red'
      | 'green'
      | 'yellow'
      | 'blue'
      | 'magenta'
      | 'cyan'
      | 'white'
      | 'gray',

    /**
    Disable the spinner and all log text. All output is suppressed and `isEnabled` will be considered `false`.
    @default false
    */
    +isSilent?: boolean,
  }>;

  declare type Ora = $ReadOnly<{
    start(text?: string): Ora,

    stop(): Ora,
  }>;

  declare function ora(options?: string | Options): Ora;
  declare module.exports: typeof ora;
}
