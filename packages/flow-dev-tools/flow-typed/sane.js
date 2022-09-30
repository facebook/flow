/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

declare module 'sane' {
  import type {Stats} from 'fs';
  declare type Sane$Options = {
    glob?: Array<string>,
    poll?: boolean,
    watchman?: boolean,
    dot?: boolean,
  };
  declare class Sane$Watcher {
    on(event: 'ready', callback: () => mixed): void;
    on(
      event: 'change',
      callback: (filepath: string, root: string, stat: Stats) => mixed,
    ): void;
    on(
      event: 'add',
      callback: (filepath: string, root: string, stat: Stats) => mixed,
    ): void;
    on(
      event: 'delete',
      callback: (filepath: string, root: string) => mixed,
    ): void;
  }
  declare export type SaneWatcher = Sane$Watcher;
  declare module.exports: (
    path: string,
    options?: Sane$Options,
  ) => Sane$Watcher;
}
