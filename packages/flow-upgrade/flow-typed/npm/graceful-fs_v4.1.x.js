// flow-typed signature: dddffa88c6bf519f7077a44e489c05c9
// flow-typed version: 1b3bf95731/graceful-fs_v4.1.x/flow_>=v0.28.x

declare module "graceful-fs" {
  declare interface ErrnoError extends Error {
    errno?: number;
    code?: string;
    path?: string;
    syscall?: string;
  }
  declare class Stats {
    dev: number;
    ino: number;
    mode: number;
    nlink: number;
    uid: number;
    gid: number;
    rdev: number;
    size: number;
    blksize: number;
    blocks: number;
    atime: Date;
    mtime: Date;
    ctime: Date;

    isFile(): boolean;
    isDirectory(): boolean;
    isBlockDevice(): boolean;
    isCharacterDevice(): boolean;
    isSymbolicLink(): boolean;
    isFIFO(): boolean;
    isSocket(): boolean;
  }

  declare class FSWatcher extends events$EventEmitter {
    close(): void
  }

  declare class ReadStream extends stream$Readable {
    close(): void
  }

  declare class WriteStream extends stream$Writable {
    close(): void
  }

  declare function gracefulify(fs: Object): void;
  declare function rename(oldPath: string, newPath: string, callback?: (err: ?ErrnoError) => void): void;
  declare function renameSync(oldPath: string, newPath: string): void;
  declare function ftruncate(fd: number, len: number, callback?: (err: ?ErrnoError) => void): void;
  declare function ftruncateSync(fd: number, len: number): void;
  declare function truncate(path: string, len: number, callback?: (err: ?ErrnoError) => void): void;
  declare function truncateSync(path: string, len: number): void;
  declare function chown(path: string, uid: number, gid: number, callback?: (err: ?ErrnoError) => void): void;
  declare function chownSync(path: string, uid: number, gid: number): void;
  declare function fchown(fd: number, uid: number, gid: number, callback?: (err: ?ErrnoError) => void): void;
  declare function fchownSync(fd: number, uid: number, gid: number): void;
  declare function lchown(path: string, uid: number, gid: number, callback?: (err: ?ErrnoError) => void): void;
  declare function lchownSync(path: string, uid: number, gid: number): void;
  declare function chmod(path: string, mode: number | string, callback?: (err: ?ErrnoError) => void): void;
  declare function chmodSync(path: string, mode: number | string): void;
  declare function fchmod(fd: number, mode: number | string, callback?: (err: ?ErrnoError) => void): void;
  declare function fchmodSync(fd: number, mode: number | string): void;
  declare function lchmod(path: string, mode: number | string, callback?: (err: ?ErrnoError) => void): void;
  declare function lchmodSync(path: string, mode: number | string): void;
  declare function stat(path: string, callback?: (err: ?ErrnoError, stats: Stats) => any): void;
  declare function statSync(path: string): Stats;
  declare function fstat(fd: number, callback?: (err: ?ErrnoError, stats: Stats) => any): void;
  declare function fstatSync(fd: number): Stats;
  declare function lstat(path: string, callback?: (err: ?ErrnoError, stats: Stats) => any): void;
  declare function lstatSync(path: string): Stats;
  declare function link(srcpath: string, dstpath: string, callback?: (err: ?ErrnoError) => void): void;
  declare function linkSync(srcpath: string, dstpath: string): void;
  declare function symlink(srcpath: string, dtspath: string, type?: string, callback?: (err: ?ErrnoError) => void): void;
  declare function symlinkSync(srcpath: string, dstpath: string, type: string): void;
  declare function readlink(path: string, callback: (err: ?ErrnoError, linkString: string) => void): void;
  declare function readlinkSync(path: string): string;
  declare function realpath(path: string, cache?: Object, callback?: (err: ?ErrnoError, resolvedPath: string) => void): void;
  declare function realpathSync(path: string, cache?: Object): string;
  declare function unlink(path: string, callback?: (err: ?ErrnoError) => void): void;
  declare function unlinkSync(path: string): void;
  declare function rmdir(path: string, callback?: (err: ?ErrnoError) => void): void;
  declare function rmdirSync(path: string): void;
  declare function mkdir(path: string, mode: number, callback: (err: ?ErrnoError) => void): void;
  declare function mkdir(path: string, callback: (err: ?ErrnoError) => void): void;
  declare function mkdirSync(path: string, mode?: number): void;
  declare function mkdtemp(prefix: string, callback: (err: ?ErrnoError, folderPath: string) => void): void;
  declare function mkdtempSync(prefix: string): string;
  declare function readdir(path: string, callback: (err: ?ErrnoError, files: Array<string>) => void): void;
  declare function readdir(
    path: string,
    options: string | { encoding: string },
    callback: (err: ?ErrnoError, files: Array<string>) => void): void;
  declare function readdirSync(path: string, options?: string | { encoding: string }, ): Array<string>;
  declare function close(fd: number, callback?: (err: ?ErrnoError) => void): void;
  declare function closeSync(fd: number): void;
  declare function open(
    path: string | Buffer,
    flags: string | number,
    mode: number,
    callback: (err: ?ErrnoError, fd: number) => void
  ): void;
  declare function open(
    path: string | Buffer,
    flags: string | number,
    callback: (err: ?ErrnoError, fd: number) => void
  ): void;
  declare function openSync(path: string | Buffer, flags: string | number, mode?: number): number;
  declare function utimes(path: string, atime: number, mtime: number, callback?: (err: ?ErrnoError) => void): void;
  declare function utimesSync(path: string, atime: number, mtime: number): void;
  declare function futimes(fd: number, atime: number, mtime: number, callback?: (err: ?ErrnoError) => void): void;
  declare function futimesSync(fd: number, atime: number, mtime: number): void;
  declare function fsync(fd: number, callback?: (err: ?ErrnoError) => void): void;
  declare function fsyncSync(fd: number): void;
  declare var write: (fd: number, buffer: Buffer, offset: number, length: number, position?: mixed, callback?: (err: ?ErrnoError, write: number, str: string) => void) => void
                   | (fd: number, data: mixed, position?: mixed, encoding?: string, callback?: (err: ?ErrnoError, write: number, str: string) => void) => void;
  declare var writeSync: (fd: number, buffer: Buffer, offset: number, length: number, position?: number) => number
                       | (fd: number, data: mixed, position?: mixed, encoding?: string) => number;
  declare function read(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: ?number,
    callback?: (err: ?ErrnoError, bytesRead: number, buffer: Buffer) => void
  ): void;
  declare function readSync(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: number
  ): number;
  declare function readFile(
    filename: string,
    callback: (err: ?ErrnoError, data: Buffer) => void
  ): void;
  declare function readFile(
    filename: string,
    encoding: string,
    callback: (err: ?ErrnoError, data: string) => void
  ): void;
  declare function readFile(
    filename: string,
    options: { encoding: string; flag?: string },
    callback: (err: ?ErrnoError, data: string) => void
  ): void;
  declare function readFile(
    filename: string,
    options: { flag?: string },
    callback: (err: ?ErrnoError, data: Buffer) => void
  ): void;
  declare function readFileSync(filename: string, _: void): Buffer;
  declare function readFileSync(filename: string, encoding: string): string;
  declare function readFileSync(filename: string, options: { encoding: string, flag?: string }): string;
  declare function readFileSync(filename: string, options: { encoding?: void, flag?: string }): Buffer;
  declare function writeFile(
    filename: string,
    data: Buffer | string,
    options: Object | string,
    callback?: (err: ?ErrnoError) => void
  ): void;
  declare function writeFile(
    filename: string,
    data: Buffer | string,
    callback?: (err: ?ErrnoError) => void
  ): void;
  declare function writeFileSync(
    filename: string,
    data: Buffer | string,
    options?: Object | string
  ): void;
  declare function appendFile(
    filename: string,
    data: string | Buffer,
    callback: (err: ?ErrnoError) => void
  ): void;
  declare function appendFile(
    filename: string,
    data: string | Buffer,
    options: string | Object,
    callback: (err: ?ErrnoError) => void
  ): void;
  declare function appendFileSync(filename: string, data: string | Buffer, options?: Object): void;
  declare function watchFile(filename: string, options?: Object, listener?: (curr: Stats, prev: Stats) => void): void;
  declare function unwatchFile(filename: string, listener?: (curr: Stats, prev: Stats) => void): void;
  declare function watch(filename: string, options?: Object, listener?: (event: string, filename: string) => void): FSWatcher;
  declare function exists(path: string, callback?: (exists: boolean) => void): void;
  declare function existsSync(path: string): boolean;
  declare function access(path: string, mode?: any, callback?: (err: ?ErrnoError) => void): void;
  declare function accessSync(path: string, mode?: any): void;
  declare function createReadStream(path: string, options?: Object): ReadStream;
  declare function createWriteStream(path: string, options?: Object): WriteStream;

  declare var F_OK: number;
  declare var R_OK: number;
  declare var W_OK: number;
  declare var X_OK: number;
}
