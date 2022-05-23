// flow-typed signature: 1cd9658b96c1beee61a7cabd7165c76a
// flow-typed version: d81afd5307/fs-extra_v10.x.x/flow_>=v0.104.x

declare module "fs-extra" {
  import type { Stats, ReadStream, WriteStream } from "fs";
  import typeof fsTypes from "fs";

  declare type SymlinkType = "dir" | "file";
  declare type FsSymlinkType = "dir" | "file" | "junction";

  declare type CopyFilterSync = (src: string, dest: string) => boolean;
  declare type CopyFilterAsync = (
    src: string,
    dest: string
  ) => Promise<boolean>;

  declare type CopyOptions = {
    dereference?: boolean,
    overwrite?: boolean,
    preserveTimestamps?: boolean,
    errorOnExist?: boolean,
    recursive?: boolean,
    ...
  };

  declare type CopyOptionsAync = CopyOptions & { filter?: CopyFilterSync | CopyFilterAsync, ... };

  declare type CopyOptionsSync = CopyOptions & { filter?: CopyFilterSync, ... };

  declare type MoveOptions = {
    overwrite?: boolean,
    limit?: number,
    ...
  };

  declare type ReadOptions = {
    throws?: boolean,
    fs?: Object,
    reviver?: any,
    encoding?: string,
    flag?: string,
    ...
  };

  declare type WriteFileOptions = {
    encoding?: string,
    flag?: string,
    mode?: number,
    ...
  };

  declare type WriteOptions = WriteFileOptions & {
    fs?: Object,
    replacer?: any,
    spaces?: number | string,
    EOL?: string,
    ...
  };

  declare type ReadResult = {
    bytesRead: number,
    buffer: Buffer,
    ...
  };

  declare type WriteResult = {
    bytesWritten: number,
    buffer: Buffer,
    ...
  };

  declare type ReadStreamOptions = {
    bufferSize?: number,
    encoding?: string,
    fd?: number,
    flags?: string,
    mode?: number,
    ...
  }

  declare type WriteStreamOptions = {
    encoding?: string,
    flags?: string,
    string?: string,
    ...
  }

  declare function copy(
    src: string,
    dest: string,
    options?: CopyOptionsAync
  ): Promise<void>;
  declare function copy(
    src: string,
    dest: string,
    callback: (err: Error) => void
  ): void;
  declare function copy(
    src: string,
    dest: string,
    options: CopyOptionsAync,
    callback: (err: Error) => void
  ): void;
  declare function copySync(
    src: string,
    dest: string,
    options?: CopyOptionsSync
  ): void;

  declare function move(
    src: string,
    dest: string,
    options?: MoveOptions
  ): Promise<void>;
  declare function move(
    src: string,
    dest: string,
    callback: (err: Error) => void
  ): void;
  declare function move(
    src: string,
    dest: string,
    options: MoveOptions,
    callback: (err: Error) => void
  ): void;
  declare function moveSync(
    src: string,
    dest: string,
    options?: MoveOptions
  ): void;

  declare function createFile(file: string): Promise<void>;
  declare function createFile(
    file: string,
    callback: (err: Error) => void
  ): void;
  declare function createFileSync(file: string): void;
  declare function createReadStream(path: string, options?: ReadStreamOptions): ReadStream;
  declare function createWriteStream(path: string, options?: WriteStreamOptions): WriteStream;

  declare function ensureDir(path: string): Promise<void>;
  declare function ensureDir(
    path: string,
    callback: (err: Error) => void
  ): void;
  declare function ensureDirSync(path: string): void;

  declare function exists(path: string): Promise<boolean>;
  declare function exists(path: string, callback?: (exists: boolean) => void): void;

  declare function mkdirs(dir: string): Promise<void>;
  declare function mkdirs(
    dir: string,
    callback: (err: Error) => void
  ): void;
  declare function mkdirsSync(dir: string): void;

  declare function mkdirp(dir: string): Promise<void>;
  declare function mkdirp(
    dir: string,
    callback: (err: Error) => void
  ): void;
  declare function mkdirpSync(dir: string): void;

  declare function outputFile(
    file: string,
    data: any,
    options?: WriteFileOptions | string
  ): Promise<void>;
  declare function outputFile(
    file: string,
    data: any,
    callback: (err: Error) => void
  ): void;
  declare function outputFile(
    file: string,
    data: any,
    options: WriteFileOptions | string,
    callback: (err: Error) => void
  ): void;
  declare function outputFileSync(
    file: string,
    data: any,
    options?: WriteFileOptions | string
  ): void;

  declare function readJson(
    file: string,
    options?: ReadOptions
  ): Promise<any>;
  declare function readJson(
    file: string,
    callback: (err: Error, jsonObject: any) => void
  ): void;
  declare function readJson(
    file: string,
    options: ReadOptions,
    callback: (err: Error, jsonObject: any) => void
  ): void;
  declare function readJSON(
    file: string,
    options?: ReadOptions
  ): Promise<any>;
  declare function readJSON(
    file: string,
    callback: (err: Error, jsonObject: any) => void
  ): void;
  declare function readJSON(
    file: string,
    options: ReadOptions,
    callback: (err: Error, jsonObject: any) => void
  ): void;

  declare function readJsonSync(
    file: string,
    options?: ReadOptions
  ): any;
  declare function readJSONSync(
    file: string,
    options?: ReadOptions
  ): any;

  declare function remove(dir: string): Promise<void>;
  declare function remove(
    dir: string,
    callback: (err: Error) => void
  ): void;
  declare function removeSync(dir: string): void;

  declare function outputJson(
    file: string,
    data: any,
    options?: WriteOptions
  ): Promise<void>;
  declare function outputJSON(
    file: string,
    data: any,
    options?: WriteOptions
  ): Promise<void>;
  declare function outputJson(
    file: string,
    data: any,
    options: WriteOptions,
    callback: (err: Error) => void
  ): void;
  declare function outputJSON(
    file: string,
    data: any,
    options: WriteOptions,
    callback: (err: Error) => void
  ): void;
  declare function outputJson(
    file: string,
    data: any,
    callback: (err: Error) => void
  ): void;
  declare function outputJSON(
    file: string,
    data: any,
    callback: (err: Error) => void
  ): void;
  declare function outputJsonSync(
    file: string,
    data: any,
    options?: WriteOptions
  ): void;
  declare function outputJSONSync(
    file: string,
    data: any,
    options?: WriteOptions
  ): void;

  declare function writeJSON(
    file: string,
    object: any,
    options?: WriteOptions
  ): Promise<void>;
  declare function writeJSON(
    file: string,
    object: any,
    callback: (err: Error) => void
  ): void;
  declare function writeJSON(
    file: string,
    object: any,
    options: WriteOptions,
    callback: (err: Error) => void
  ): void;
  declare function writeJson(
    file: string,
    object: any,
    options?: WriteOptions
  ): Promise<void>;
  declare function writeJson(
    file: string,
    object: any,
    callback: (err: Error) => void
  ): void;
  declare function writeJson(
    file: string,
    object: any,
    options: WriteOptions,
    callback: (err: Error) => void
  ): void;

  declare function writeJsonSync(
    file: string,
    object: any,
    options?: WriteOptions
  ): void;
  declare function writeJSONSync(
    file: string,
    object: any,
    options?: WriteOptions
  ): void;

  declare function ensureFile(path: string): Promise<void>;
  declare function ensureFile(
    path: string,
    callback: (err: Error) => void
  ): void;
  declare function ensureFileSync(path: string): void;

  declare function ensureLink(src: string, dest: string): Promise<void>;
  declare function ensureLink(
    src: string,
    dest: string,
    callback: (err: Error) => void
  ): void;
  declare function ensureLinkSync(src: string, dest: string): void;

  declare function ensureSymlink(
    src: string,
    dest: string,
    type?: SymlinkType
  ): Promise<void>;
  declare function ensureSymlink(
    src: string,
    dest: string,
    type: SymlinkType,
    callback: (err: Error) => void
  ): void;
  declare function ensureSymlink(
    src: string,
    dest: string,
    callback: (err: Error) => void
  ): void;
  declare function ensureSymlinkSync(
    src: string,
    dest: string,
    type?: SymlinkType
  ): void;

  declare function emptyDir(path: string): Promise<void>;
  declare function emptyDir(
    path: string,
    callback: (err: Error) => void
  ): void;
  declare function emptyDirSync(path: string): void;

  declare function pathExists(path: string): Promise<boolean>;
  declare function pathExists(
    path: string,
    callback: (err: Error, exists: boolean) => void
  ): void;
  declare function pathExistsSync(path: string): boolean;

  declare function access(
    path: string | Buffer,
    callback: (err: ErrnoError) => void
  ): void;
  declare function access(
    path: string | Buffer,
    mode: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function access(
    path: string | Buffer,
    mode?: number
  ): Promise<void>;

  declare function appendFile(
    file: string | Buffer | number,
    data: any,
    options: {
      encoding?: string,
      mode?: number | string,
      flag?: string,
      ...
    },
    callback: (err: ErrnoError) => void
  ): void;
  declare function appendFile(
    file: string | Buffer | number,
    data: any,
    callback: (err: ErrnoError) => void
  ): void;
  declare function appendFile(
    file: string | Buffer | number,
    data: any,
    options?: {
      encoding?: string,
      mode?: number | string,
      flag?: string,
      ...
    }
  ): Promise<void>;

  declare function chmod(
    path: string | Buffer,
    mode: string | number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function chmod(
    path: string | Buffer,
    mode: string | number
  ): Promise<void>;

  declare function chown(
    path: string | Buffer,
    uid: number,
    gid: number
  ): Promise<void>;
  declare function chown(
    path: string | Buffer,
    uid: number,
    gid: number,
    callback: (err: ErrnoError) => void
  ): void;

  declare function close(
    fd: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function close(fd: number): Promise<void>;

  declare function fchmod(
    fd: number,
    mode: string | number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function fchmod(
    fd: number,
    mode: string | number
  ): Promise<void>;

  declare function fchown(
    fd: number,
    uid: number,
    gid: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function fchown(
    fd: number,
    uid: number,
    gid: number
  ): Promise<void>;

  declare function fdatasync(fd: number, callback: () => void): void;
  declare function fdatasync(fd: number): Promise<void>;

  declare function fstat(
    fd: number,
    callback: (err: ErrnoError, stats: Stats) => any
  ): void;
  declare function fstat(fd: number): Promise<Stats>;

  declare function fsync(
    fd: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function fsync(fd: number): Promise<void>;

  declare function ftruncate(
    fd: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function ftruncate(
    fd: number,
    len: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function ftruncate(fd: number, len?: number): Promise<void>;

  declare function futimes(
    fd: number,
    atime: number,
    mtime: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function futimes(
    fd: number,
    atime: Date,
    mtime: Date,
    callback: (err: ErrnoError) => void
  ): void;
  declare function futimes(
    fd: number,
    atime: number,
    mtime: number
  ): Promise<void>;
  declare function futimes(
    fd: number,
    atime: Date,
    mtime: Date
  ): Promise<void>;

  declare function lchown(
    path: string | Buffer,
    uid: number,
    gid: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function lchown(
    path: string | Buffer,
    uid: number,
    gid: number
  ): Promise<void>;

  declare function link(
    srcpath: string | Buffer,
    dstpath: string | Buffer,
    callback: (err: ErrnoError) => void
  ): void;
  declare function link(
    srcpath: string | Buffer,
    dstpath: string | Buffer
  ): Promise<void>;

  declare function lstat(
    path: string | Buffer,
    callback: (err: ErrnoError, stats: Stats) => any
  ): void;
  declare function lstat(path: string | Buffer): Promise<Stats>;

  declare function mkdir(
    path: string | Buffer,
    callback: (err: ErrnoError) => void
  ): void;
  declare function mkdir(
    path: string | Buffer,
    mode: number | string,
    callback: (err: ErrnoError) => void
  ): void;
  declare function mkdir(path: string | Buffer): Promise<void>;

  declare function open(
    path: string | Buffer,
    flags: string | number,
    callback: (err: ErrnoError, fd: number) => void
  ): void;
  declare function open(
    path: string | Buffer,
    flags: string | number,
    mode: number,
    callback: (err: ErrnoError, fd: number) => void
  ): void;
  declare function open(
    path: string | Buffer,
    flags: string | number,
    mode?: number
  ): Promise<number>;

  declare function read(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: number | null,
    callback: (err: ErrnoError, bytesRead: number, buffer: Buffer) => void
  ): void;
  declare function read(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: number | null
  ): Promise<ReadResult>;

  declare function readFile(
    file: string | Buffer | number,
    callback: (err: ErrnoError, data: Buffer) => void
  ): void;
  declare function readFile(
    file: string | Buffer | number,
    encoding: string,
    callback: (err: ErrnoError, data: string) => void
  ): void;
  declare function readFile(
    file: string | Buffer | number,
    options: { flag?: string, ... } | {
      encoding: string,
      flag?: string,
      ...
    },
    callback: (err: ErrnoError, data: Buffer) => void
  ): void;
  declare function readFile(
    file: string | Buffer | number,
    options: { flag?: string, ... } | {
      encoding: string,
      flag?: string,
      ...
    },
  ): Promise<string>;
  declare function readFile(
    file: string | Buffer | number,
    encoding: string
  ): Promise<string>;
  declare function readFile(
    file: string | Buffer | number
  ): Promise<Buffer>;

  declare function readdir(
    path: string | Buffer,
    callback: (err: ErrnoError, files: string[]) => void
  ): void;
  declare function readdir(path: string | Buffer): Promise<string[]>;

  declare function readlink(
    path: string | Buffer,
    callback: (err: ErrnoError, linkString: string) => any
  ): void;
  declare function readlink(path: string | Buffer): Promise<string>;

  declare function realpath(
    path: string | Buffer,
    callback: (err: ErrnoError, resolvedPath: string) => any
  ): void;
  declare function realpath(
    path: string | Buffer,
    cache: { [path: string]: string, ... },
    callback: (err: ErrnoError, resolvedPath: string) => any
  ): void;
  declare function realpath(
    path: string | Buffer,
    cache?: { [path: string]: string, ... }
  ): Promise<string>;

  declare function rename(
    oldPath: string,
    newPath: string,
    callback: (err: ErrnoError) => void
  ): void;
  declare function rename(
    oldPath: string,
    newPath: string
  ): Promise<void>;

  declare function rmdir(
    path: string | Buffer,
    callback: (err: ErrnoError) => void
  ): void;
  declare function rmdir(path: string | Buffer): Promise<void>;

  declare function stat(
    path: string | Buffer,
    callback: (err: ErrnoError, stats: Stats) => any
  ): void;
  declare function stat(path: string | Buffer): Promise<Stats>;

  declare function statSync(path: string): Stats;

  declare function symlink(
    srcpath: string | Buffer,
    dstpath: string | Buffer,
    type: FsSymlinkType | void,
    callback: (err: ErrnoError) => void
  ): void;
  declare function symlink(
    srcpath: string | Buffer,
    dstpath: string | Buffer,
    callback: (err: ErrnoError) => void
  ): void;
  declare function symlink(
    srcpath: string | Buffer,
    dstpath: string | Buffer,
    type?: FsSymlinkType
  ): Promise<void>;

  declare function truncate(
    path: string | Buffer,
    callback: (err: ErrnoError) => void
  ): void;
  declare function truncate(
    path: string | Buffer,
    len: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function truncate(
    path: string | Buffer,
    len?: number
  ): Promise<void>;

  declare function unlink(
    path: string | Buffer,
    callback: (err: ErrnoError) => void
  ): void;
  declare function unlink(path: string | Buffer): Promise<void>;

  declare function utimes(
    path: string | Buffer,
    atime: number,
    mtime: number,
    callback: (err: ErrnoError) => void
  ): void;
  declare function utimes(
    path: string | Buffer,
    atime: Date,
    mtime: Date,
    callback: (err: ErrnoError) => void
  ): void;
  declare function utimes(
    path: string | Buffer,
    atime: number,
    mtime: number
  ): Promise<void>;
  declare function utimes(
    path: string | Buffer,
    atime: Date,
    mtime: Date
  ): Promise<void>;

  declare function write(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: number | null,
    callback: (err: ErrnoError, written: number, buffer: Buffer) => void
  ): void;
  declare function write(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    callback: (err: ErrnoError, written: number, buffer: Buffer) => void
  ): void;
  declare function write(
    fd: number,
    data: any,
    callback: (err: ErrnoError, written: number, str: string) => void
  ): void;
  declare function write(
    fd: number,
    data: any,
    offset: number,
    callback: (err: ErrnoError, written: number, str: string) => void
  ): void;
  declare function write(
    fd: number,
    data: any,
    offset: number,
    encoding: string,
    callback: (err: ErrnoError, written: number, str: string) => void
  ): void;
  declare function write(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position?: number | null
  ): Promise<WriteResult>;
  declare function write(
    fd: number,
    data: any,
    offset: number,
    encoding?: string
  ): Promise<WriteResult>;

  declare function writeFile(
    file: string | Buffer | number,
    data: any,
    callback: (err: ErrnoError) => void
  ): void;
  declare function writeFile(
    file: string | Buffer | number,
    data: any,
    options?: WriteFileOptions | string
  ): Promise<void>;
  declare function writeFile(
    file: string | Buffer | number,
    data: any,
    options: WriteFileOptions | string,
    callback: (err: ErrnoError) => void
  ): void;

  declare function mkdtemp(prefix: string): Promise<string>;
  declare function mkdtemp(
    prefix: string,
    callback: (err: ErrnoError, folder: string) => void
  ): void;

  declare module.exports: {|
    ...fsTypes;
    access: typeof access;
    appendFile: typeof appendFile;
    chmod: typeof chmod;
    chown: typeof chown;
    close: typeof close;
    copy: typeof copy;
    copySync: typeof copySync;
    createFile: typeof createFile;
    createFileSync: typeof createFileSync;
    createReadStream: typeof createReadStream;
    createWriteStream: typeof createWriteStream;
    emptyDir: typeof emptyDir;
    emptyDirSync: typeof emptyDirSync;
    ensureDir: typeof ensureDir;
    ensureDirSync: typeof ensureDirSync;
    ensureFile: typeof ensureFile;
    ensureFileSync: typeof ensureFileSync;
    ensureLink: typeof ensureLink;
    ensureLinkSync: typeof ensureLinkSync;
    ensureSymlink: typeof ensureSymlink;
    ensureSymlinkSync: typeof ensureSymlinkSync;
    exists: typeof exists;
    fchmod: typeof fchmod;
    fchown: typeof fchown;
    fdatasync: typeof fdatasync;
    fstat: typeof fstat;
    fsync: typeof fsync;
    ftruncate: typeof ftruncate;
    futimes: typeof futimes;
    lchown: typeof lchown;
    link: typeof link;
    lstat: typeof lstat;
    mkdir: typeof mkdir;
    mkdirp: typeof mkdirp;
    mkdirpSync: typeof mkdirpSync;
    mkdirs: typeof mkdirs;
    mkdirsSync: typeof mkdirsSync;
    mkdtemp: typeof mkdtemp;
    move: typeof move;
    moveSync: typeof moveSync;
    open: typeof open;
    outputFile: typeof outputFile;
    outputFileSync: typeof outputFileSync;
    outputJson: typeof outputJson;
    outputJSON: typeof outputJSON;
    outputJsonSync: typeof outputJsonSync;
    outputJSONSync: typeof outputJSONSync;
    pathExists: typeof pathExists;
    pathExistsSync: typeof pathExistsSync;
    read: typeof read;
    readdir: typeof readdir;
    readFile: typeof readFile;
    readJson: typeof readJson;
    readJSON: typeof readJSON;
    readJsonSync: typeof readJsonSync;
    readJSONSync: typeof readJSONSync;
    readlink: typeof readlink;
    realpath: typeof realpath;
    remove: typeof remove;
    removeSync: typeof removeSync;
    rename: typeof rename;
    rmdir: typeof rmdir;
    stat: typeof stat;
    statSync: typeof statSync;
    symlink: typeof symlink;
    truncate: typeof truncate;
    unlink: typeof unlink;
    utimes: typeof utimes;
    write: typeof write;
    writeFile: typeof writeFile;
    writeJSON: typeof writeJSON;
    writeJson: typeof writeJson;
    writeJsonSync: typeof writeJsonSync;
    writeJSONSync: typeof writeJSONSync;
  |};
}
