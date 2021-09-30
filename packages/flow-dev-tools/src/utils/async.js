/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {exec: cp_exec} = require('child_process');
const {createInterface: rl_createInterface} = require('readline');
const {
  appendFile: fs_appendFile,
  exists: fs_exists,
  readdir: fs_readdir,
  readFile: fs_readFile,
  rename: fs_rename,
  symlink: fs_symlink,
  unlink: fs_unlink,
  writeFile: fs_writeFile,
} = require('fs');
const {ncp: ncp_ncp} = require('ncp');
const {format} = require('util');
const {glob: glob_glob} = require('glob');
const mkdirp_mkdirp = require('mkdirp');
const rimraf_rimraf = require('rimraf');

const {splitIntoChunks} = require('./string');

import type {ReadStream, WriteStream} from 'fs';

export type ExecOpts = child_process$execOpts & {
  stdin?: string,
};

// Based on nothing but a few experiments on my laptop,
// this seems like a pretty safe size.
const STDIN_WRITE_CHUNK_SIZE = 10000;

function exec(cmd: string, options?: ExecOpts): Promise<string> {
  return new Promise((resolve, reject) => {
    const cp = cp_exec(cmd, options, (err, stdout, stderr) => {
      if (err == null) {
        resolve(stdout.toString());
      } else {
        reject([err, stdout, stderr]);
      }
    });
    if (options != null && options.stdin != null) {
      // If we just write a giant string it can lead to a crash
      const chunks = splitIntoChunks(options.stdin, STDIN_WRITE_CHUNK_SIZE);
      const write = (chunkIndex: number) => {
        if (chunkIndex >= chunks.length) {
          cp.stdin.end();
          return;
        }
        const canContinue = cp.stdin.write(chunks[chunkIndex], 'utf8');
        if (canContinue) {
          write(chunkIndex + 1);
        } else {
          cp.stdin.once('drain', () => write(chunkIndex + 1));
        }
      };
      write(0);
    }
  });
}

function execManual(
  cmd: string,
  options?: Object,
): Promise<[?Object, string | Buffer, string | Buffer]> {
  return new Promise((resolve, reject) =>
    cp_exec(cmd, options, (err, stdout, stderr) =>
      resolve([err, stdout, stderr]),
    ),
  );
}

type WriteFileOptions = {
  encoding?: string | null,
  mode?: number,
  flag?: string,
};
function writeFile(
  filename: string,
  data: string,
  options?: WriteFileOptions = {},
): Promise<void> {
  return new Promise((resolve, reject) => {
    fs_writeFile(filename, data, options, err => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    });
  });
}

function appendFile(filename: string, data: string): Promise<void> {
  return new Promise((resolve, reject) => {
    fs_appendFile(filename, data, err => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    });
  });
}

function readFile(filename: string): Promise<string> {
  return new Promise((resolve, reject) => {
    fs_readFile(filename, 'utf-8', (err, data) => {
      if (err == null) {
        // Even if we check out the files without CRLF, reading seems to add it
        // in.
        resolve(data.replace(/\r\n/g, '\n'));
      } else {
        reject(err);
      }
    });
  });
}

function readdir(dir: string): Promise<Array<string>> {
  return new Promise((resolve, reject) => {
    fs_readdir(dir, (err, data) => {
      if (err == null) {
        resolve(data);
      } else {
        reject(err);
      }
    });
  });
}

function rename(old_path: string, new_path: string): Promise<void> {
  return new Promise((resolve, reject) => {
    fs_rename(old_path, new_path, err => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    });
  });
}

function rimraf(path: string): Promise<void> {
  return new Promise((resolve, reject) => {
    rimraf_rimraf(path, err => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    });
  });
}

function unlink(file: string): Promise<void> {
  return new Promise((resolve, reject) => {
    fs_unlink(file, err => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    });
  });
}

function mkdirp(dir: string): Promise<void> {
  return new Promise((resolve, reject) => {
    mkdirp_mkdirp(dir, err => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
}

export type NCPFile = {
  name: string,
  mode: number,
  mtime: Date,
  atime: Date,
};

type NCPOptions = {
  filter?: RegExp | ((filename: string) => boolean),
  transform?: (read: ReadStream, write: WriteStream, file: NCPFile) => mixed,
  clobber?: boolean,
  dereference?: boolean,
  stopOnErr?: boolean,
  errs?: any,
};
function ncp(
  source: string,
  dest: string,
  options?: NCPOptions,
): Promise<void> {
  return new Promise((resolve, reject) => {
    ncp_ncp(source, dest, options || {}, err => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
}

function drain(writer: stream$Writable | tty$WriteStream): Promise<void> {
  return new Promise((resolve, reject) => {
    writer.once('drain', resolve);
  });
}

function exists(path: string): Promise<boolean> {
  return new Promise((resolve, reject) => {
    fs_exists(path, resolve);
  });
}

function symlink(
  target: string | Buffer,
  path: string | Buffer,
): Promise<void> {
  return new Promise((resolve, reject) => {
    // $FlowIssue - symlink can omit the type
    fs_symlink(target.toString(), path.toString(), resolve);
  });
}

type GlobOptions = {
  cwd?: string,
  nodir?: boolean,
  dot?: boolean,
};
function glob(pattern: string, options: GlobOptions): Promise<Array<string>> {
  return new Promise((resolve, reject) => {
    glob_glob(pattern, options, (err, files) => {
      if (err) {
        reject(err);
      } else {
        resolve(files);
      }
    });
  });
}

function isRunning(pid: number): Promise<boolean> {
  return new Promise(resolve => {
    try {
      process.kill(pid, 0);
      resolve(true);
    } catch (e) {
      resolve(e.code === 'EPERM');
    }
  });
}

function sleep(timeoutMs: number): Promise<void> {
  return new Promise(resolve => {
    setTimeout(resolve, timeoutMs);
  });
}

function prompt(message: string): Promise<string> {
  const rl = rl_createInterface({
    input: process.stdin,
    output: process.stdout,
  });
  return new Promise(resolve => {
    rl.question(message, result => {
      rl.close();
      resolve(result);
    });
  });
}

function withTimeout<A, B>(
  timeout_ms: number,
  promise: Promise<A>,
  onTimeout: () => B,
): Promise<A | B> {
  let timer;
  const timeout = new Promise(resolve => {
    timer = setTimeout(() => resolve(onTimeout()), timeout_ms);
  });
  return Promise.race([timeout, promise]).finally(
    () => timer && clearTimeout(timer),
  );
}

module.exports = {
  exec,
  execManual,
  writeFile,
  appendFile,
  readFile,
  readdir,
  rename,
  rimraf,
  unlink,
  mkdirp,
  ncp,
  drain,
  exists,
  symlink,
  glob,
  isRunning,
  sleep,
  prompt,
  withTimeout,
};
