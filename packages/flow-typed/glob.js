/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

// adapted from https://github.com/flow-typed/flow-typed/blob/e4ff154d05f199703a1611b44fb23b6950c324c8/definitions/npm/glob_v7.x.x/flow_v0.104.x-/glob_v7.x.x.js

declare module 'glob' {
  declare type MinimatchOptions = {
    debug?: boolean,
    nobrace?: boolean,
    noglobstar?: boolean,
    dot?: boolean,
    noext?: boolean,
    nocase?: boolean,
    nonull?: boolean,
    matchBase?: boolean,
    nocomment?: boolean,
    nonegate?: boolean,
    flipNegate?: boolean,
  };

  declare type Options = {
    ...MinimatchOptions,
    cwd?: string,
    root?: string,
    nomount?: boolean,
    mark?: boolean,
    nosort?: boolean,
    stat?: boolean,
    silent?: boolean,
    strict?: boolean,
    cache?: {
      [path: string]: boolean | 'DIR' | 'FILE' | $ReadOnlyArray<string>,
      ...
    },
    statCache?: {
      [path: string]: boolean | {isDirectory(): boolean, ...} | void,
      ...
    },
    symlinks?: {[path: string]: boolean | void, ...},
    realpathCache?: {[path: string]: string, ...},
    sync?: boolean,
    nounique?: boolean,
    nodir?: boolean,
    ignore?: string | $ReadOnlyArray<string>,
    follow?: boolean,
    realpath?: boolean,
    absolute?: boolean,
  };

  /**
   * Called when an error occurs, or matches are found
   *   err
   *   matches: filenames found matching the pattern
   */
  declare type CallBack = (err: ?Error, matches: Array<string>) => void;

  declare class Glob {
    constructor(pattern: string): this;
    constructor(pattern: string, callback: CallBack): this;
    constructor(pattern: string, options: Options, callback: CallBack): this;

    minimatch: {...};
    options: Options;
    aborted: boolean;
    cache: {
      [path: string]: boolean | 'DIR' | 'FILE' | $ReadOnlyArray<string>,
      ...
    };
    statCache: {
      [path: string]: boolean | {isDirectory(): boolean, ...} | void,
      ...
    };
    symlinks: {[path: string]: boolean | void, ...};
    realpathCache: {[path: string]: string, ...};
    found: Array<string>;

    pause(): void;
    resume(): void;
    abort(): void;
  }

  declare class GlobModule {
    Glob: Class<Glob>;

    (pattern: string, callback: CallBack): void;
    (pattern: string, options: Options, callback: CallBack): void;

    hasMagic(pattern: string, options?: Options): boolean;
    sync(pattern: string, options?: Options): Array<string>;
  }

  declare module.exports: GlobModule;
}
