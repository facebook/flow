/* @flow */

import { exec as cp_exec } from 'child_process';
import {
  appendFile as fs_appendFile,
  exists as fs_exists,
  readdir as fs_readdir,
  readFile as fs_readFile,
  rename as fs_rename,
  unlink as fs_unlink,
  writeFile as fs_writeFile,
} from 'fs';
import {ncp as ncp_ncp} from 'ncp';
import {format} from 'util';
import {glob as glob_glob} from 'glob';
import mkdirp_mkdirp from 'mkdirp';
import rimraf_rimraf from 'rimraf';

export function exec(cmd: string, options?: Object): Promise<string> {
  return new Promise((resolve, reject) => {
    cp_exec(cmd, options, (err, stdout, stderr) => {
      if (err == null) {
        resolve(stdout.toString());
      } else {
        reject(err, stdout, stderr);
      }
    })
  });
}

export function execManual(cmd: string, options?: Object): Promise<[?Object, Buffer, Buffer]> {
  return new Promise((resolve, reject) =>
    cp_exec(cmd, options, (err, stdout, stderr) => resolve([err, stdout, stderr]))
  )
}


export function writeFile(filename: string, data: string): Promise<void> {
  return new Promise((resolve, reject) => {
    fs_writeFile(filename, data, (err) => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    })
  });
}

export function appendFile(filename: string, data: string): Promise<void> {
  return new Promise((resolve, reject) => {
    fs_appendFile(filename, data, (err) => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    })
  });
}

export function readFile(filename: string): Promise<string> {
  return new Promise((resolve, reject) => {
    fs_readFile(filename, 'utf-8', (err, data) => {
      if (err == null) {
        // Even if we check out the files without CRLF, reading seems to add it
        // in.
        resolve(data.replace(/\r\n/g, "\n"));
      } else {
        reject(err);
      }
    })
  });
}

export function readdir(dir: string): Promise<Array<string>> {
  return new Promise((resolve, reject) => {
    fs_readdir(dir, (err, data) => {
      if (err == null) {
        resolve(data);
      } else {
        reject(err);
      }
    })
  });
}

export function rename(old_path: string, new_path: string): Promise<void> {
  return new Promise((resolve, reject) => {
    fs_rename(old_path, new_path, (err) => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    });
  });
}

export function rimraf(path: string, options?: Object = {}): Promise<void> {
  return new Promise((resolve, reject) => {
    rimraf_rimraf(path, options, (err) => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    });
  });
}

export function unlink(file: string): Promise<void> {
  return new Promise((resolve, reject) => {
    fs_unlink(file, (err) => {
      if (err == null) {
        resolve();
      } else {
        reject(err);
      }
    });
  });
}

export function mkdirp(dir: string): Promise<void> {
  return new Promise((resolve, reject) => {
    mkdirp_mkdirp(dir, (err) => {
      if (err) {
        reject(err);
      } else {
        resolve();
      }
    });
  });
}

type NCPOptions = {
  filter?: RegExp | (filename: string) => boolean,
  transform?: (read: any, write: any) => mixed,
  clobber?: boolean,
  dereference?: boolean,
  stopOnErr?: boolean,
  errs?: any,
};
export function ncp(source: string, dest: string, options?: NCPOptions): Promise<void> {
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

export function drain(writer: stream$Writable | tty$WriteStream): Promise<void> {
  return new Promise((resolve, reject) => {
    writer.once('drain', resolve)
  });
}

export function exists(path: string): Promise<boolean> {
  return new Promise((resolve, reject) => {
    fs_exists(path, resolve);
  });
}

type GlobOptions = {
  cwd?: string,
  nodir?: boolean,
  dot?: boolean,
};
export function glob(
  pattern: string,
  options: GlobOptions,
): Promise<Array<string>> {
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
