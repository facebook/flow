/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @oncall flow
 */

declare class Buffer {
  toString(encoding?: string): string;
}

declare module 'child_process' {
  declare function execSync(command: string, options?: mixed): Buffer;

  declare function spawn(
    command: string,
    args: Array<string>,
  ): {
    stdin: {end(input: string, encoding: string): void, ...},
    stdout: AsyncIterable<Buffer>,
    ...
  };

  declare function spawnSync(
    command: string,
    args: Array<string>,
    options?: mixed,
  ): {status: number | null, ...};
}

declare module 'fs' {
  declare function existsSync(path: string): boolean;
  declare function mkdirSync(
    path: string,
    options?: {recursive?: boolean, ...},
  ): void;
  declare function readFileSync(path: string): Buffer;
  declare function writeFileSync(path: string, data: string): void;
}

declare module 'path' {
  declare function dirname(path: string): string;
}

declare var process: {
  env: {[key: string]: string | void, ...},
  ...
};
