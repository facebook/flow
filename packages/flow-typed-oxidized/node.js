/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

// Adapted from https://github.com/flow-typed/flow-typed/blob/main/definitions/environments/node/flow_v0.261.x-/node.js
// containing only what's needed for hermes-parser.

declare class URL {}

type buffer$NonBufferEncoding =
  | 'hex'
  | 'HEX'
  | 'utf8'
  | 'UTF8'
  | 'utf-8'
  | 'UTF-8'
  | 'ascii'
  | 'ASCII'
  | 'binary'
  | 'BINARY'
  | 'base64'
  | 'BASE64'
  | 'ucs2'
  | 'UCS2'
  | 'ucs-2'
  | 'UCS-2'
  | 'utf16le'
  | 'UTF16LE'
  | 'utf-16le'
  | 'UTF-16LE'
  | 'latin1';
type buffer$Encoding = buffer$NonBufferEncoding | 'buffer';

declare class Buffer extends Uint8Array {
  static from(value: Buffer): Buffer;
  static from(value: string, encoding?: buffer$Encoding): Buffer;
  static from(
    value: ArrayBuffer | SharedArrayBuffer,
    byteOffset?: number,
    length?: number,
  ): Buffer;
  static from(value: Iterable<number>): this;
}

declare module 'child_process' {
  declare type execSyncOpts = {
    cwd?: string,
    input?: string | Buffer | $TypedArray | DataView,
    stdio?: string | Array<any>,
    env?: Object,
    shell?: string,
    uid?: number,
    gid?: number,
    timeout?: number,
    killSignal?: string | number,
    maxBuffer?: number,
    encoding?: string,
    windowsHide?: boolean,
    ...
  };

  declare function execSync(
    command: string,
    options: {encoding: buffer$NonBufferEncoding, ...} & execSyncOpts,
  ): string;

  declare function execSync(command: string, options?: execSyncOpts): Buffer;
}

declare module 'fs' {
  declare function readFileSync(path: string | Buffer | URL | number): Buffer;
  declare function readFileSync(
    path: string | Buffer | URL | number,
    encoding: string,
  ): string;
  declare function readFileSync(
    path: string | Buffer | URL | number,
    options: {
      encoding: string,
      flag?: string,
      ...
    },
  ): string;
  declare function readFileSync(
    path: string | Buffer | URL | number,
    options: {
      encoding?: void,
      flag?: string,
      ...
    },
  ): Buffer;
  declare function writeFileSync(
    filename: string,
    data: Buffer | string,
    options?:
      | string
      | {
          encoding?: ?string,
          mode?: number,
          flag?: string,
          ...
        },
  ): void;
}

declare module 'os' {
  declare var EOL: string;
}

declare module 'path' {
  declare function normalize(path: string): string;
  declare function join(...parts: Array<string>): string;
  declare function resolve(...parts: Array<string>): string;
  declare function isAbsolute(path: string): boolean;
  declare function relative(from: string, to: string): string;
  declare function dirname(path: string): string;
  declare function basename(path: string, ext?: string): string;
  declare function extname(path: string): string;
  declare var sep: string;
  declare var delimiter: string;
  declare function parse(pathString: string): {
    root: string,
    dir: string,
    base: string,
    ext: string,
    name: string,
    ...
  };
  declare function format(pathObject: {
    root?: string,
    dir?: string,
    base?: string,
    ext?: string,
    name?: string,
    ...
  }): string;
  declare var posix: any;
  declare var win32: any;
}

declare var process: {
  env: {[key: string]: string | void, ...},
  argv: Array<string>,
  ...
};

declare var __dirname: string;
