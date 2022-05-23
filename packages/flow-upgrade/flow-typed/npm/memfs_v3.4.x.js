/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

// adapted from the module's source code
// https://github.com/streamich/memfs/tree/05877386a95dab801621079ced2e09a45711e4d0

declare module 'memfs' {
  declare type PathLike = string | Buffer | URL;
  declare type DirectoryContent = string | null;
  declare type symlink$Type = 'dir' | 'file' | 'junction';
  declare interface IError extends Error {
    code?: string;
  }
  declare type TDataOut = string | Buffer;
  declare type TEncodingExtended = buffer$Encoding | 'buffer';
  declare type TFileId = PathLike | number;
  declare type TData = TDataOut | Uint8Array;
  declare type TFlags = string | number;
  declare type TFlagsCopy = number;
  declare type TMode = string | number;
  declare type TTime = number | string | Date;
  declare type TCallback<TData> = (error?: IError | null, data?: TData) => void;
  declare type DirectoryJSON = {
    [key: string]: DirectoryContent,
  };
  declare type NestedDirectoryJSON = {
    [key: string]: DirectoryContent | NestedDirectoryJSON,
  };
  declare type IOptions = {
    encoding?: buffer$Encoding | TEncodingExtended,
  };
  declare type IFileOptions = {
    ...IOptions,
    mode?: TMode,
    flag?: TFlags,
  };
  declare type IReadFileOptions = {
    ...IOptions,
    flag?: string,
  };
  declare type IWriteFileOptions = IFileOptions;
  declare type IAppendFileOptions = IFileOptions;
  declare type IRealpathOptions = {
    encoding?: TEncodingExtended,
  };
  declare type IWatchFileOptions = {
    persistent?: boolean,
    interval?: number,
  };
  declare type IReadStreamOptions = {
    flags?: TFlags,
    encoding?: buffer$Encoding,
    fd?: number,
    mode?: TMode,
    autoClose?: boolean,
    start?: number,
    end?: number,
  };
  declare type IWriteStreamOptions = {
    flags?: TFlags,
    defaultEncoding?: buffer$Encoding,
    fd?: number,
    mode?: TMode,
    autoClose?: boolean,
    start?: number,
  };
  declare type IWatchOptions = {
    ...IOptions,
    persistent?: boolean,
    recursive?: boolean,
  };
  declare type IMkdirOptions = {
    mode?: TMode,
    recursive?: boolean,
  };
  declare type IRmdirOptions = {
    recursive?: boolean,
  };
  declare type IRmOptions = {
    force?: boolean,
    maxRetries?: number,
    recursive?: boolean,
    retryDelay?: number,
  };
  declare type IReaddirOptions = {
    ...IOptions,
    withFileTypes?: boolean,
  };
  declare type IStatOptions = {
    bigint?: boolean,
    throwIfNoEntry?: boolean,
  };
  declare type IFStatOptions = {
    bigint?: boolean,
  };
  declare class IReadStream extends stream$Readable {
    constructor(path: PathLike, options: IReadStreamOptions): IReadStream;
    open(): any;
    close(callback: TCallback<void>): any;
    bytesRead: number;
    path: string;
  }
  declare class IWriteStream extends stream$Writable {
    bytesWritten: number;
    path: string;
    constructor(path: PathLike, options: IWriteStreamOptions): IWriteStream;
    open(): any;
    close(): any;
  }
  /**
   * Node in a file system (like i-node, v-node).
   */
  declare class Node extends events$EventEmitter {
    ino: number;
    uid: number;
    gid: number;
    atime: Date;
    mtime: Date;
    ctime: Date;
    buf: Buffer;
    perm: number;
    mode: number;
    nlink: number;
    symlink: string[];
    constructor(ino: number, perm?: number): Node;
    getString(encoding?: string): string;
    setString(str: string): void;
    getBuffer(): Buffer;
    setBuffer(buf: Buffer): void;
    getSize(): number;
    setModeProperty(property: number): void;
    setIsFile(): void;
    setIsDirectory(): void;
    setIsSymlink(): void;
    isFile(): boolean;
    isDirectory(): boolean;
    isSymlink(): boolean;
    makeSymlink(steps: string[]): void;
    write(buf: Buffer, off?: number, len?: number, pos?: number): number;
    read(
      buf: Buffer | Uint8Array,
      off?: number,
      len?: number,
      pos?: number,
    ): number;
    truncate(len?: number): void;
    chmod(perm: number): void;
    chown(uid: number, gid: number): void;
    touch(): void;
    canRead(uid?: number, gid?: number): boolean;
    canWrite(uid?: number, gid?: number): boolean;
    del(): void;
    toJSON(): {
      ino: number,
      uid: number,
      gid: number,
      atime: number,
      mtime: number,
      ctime: number,
      perm: number,
      mode: number,
      nlink: number,
      symlink: string[],
      data: string,
    };
  }
  /**
   * Represents a hard link that points to an i-node `node`.
   */
  declare class Link extends events$EventEmitter {
    vol: Volume;
    parent: Link;
    children: {
      [child: string]: Link | void,
    };
    node: Node;
    ino: number;
    length: number;
    name: string;
    get steps(): string[];
    set steps(val: string[]): void;
    constructor(vol: Volume, parent: Link, name: string): Link;
    setNode(node: Node): void;
    getNode(): Node;
    createChild(name: string, node?: Node): Link;
    setChild(name: string, link?: Link): Link;
    deleteChild(link: Link): void;
    getChild(name: string): Link | void;
    getPath(): string;
    getName(): string;
    /**
     * Walk the tree path and return the `Link` at that location, if any.
     * @param steps {string[]} Desired location.
     * @param stop {number} Max steps to go into.
     * @param i {number} Current step in the `steps` array.
     *
     * @return {Link|null}
     */
    walk(steps: string[], stop?: number, i?: number): Link | null;
    toJSON(): {
      steps: string[],
      ino: number,
      children: string[],
    };
    syncSteps(): void;
  }
  /**
   * Represents an open file (file descriptor) that points to a `Link` (Hard-link) and a `Node`.
   */
  declare class File {
    fd: number;
    /**
     * Hard link that this file opened.
     * @type {any}
     */
    link: Link;
    /**
     * Reference to a `Node`.
     * @type {Node}
     */
    node: Node;
    /**
     * A cursor/offset position in a file, where data will be written on write.
     * User can "seek" this position.
     */
    position: number;
    flags: number;
    /**
     * Open a Link-Node pair. `node` is provided separately as that might be a different node
     * rather the one `link` points to, because it might be a symlink.
     * @param link
     * @param node
     * @param flags
     * @param fd
     */
    constructor(link: Link, node: Node, flags: number, fd: number): File;
    getString(encoding?: string): string;
    setString(str: string): void;
    getBuffer(): Buffer;
    setBuffer(buf: Buffer): void;
    getSize(): number;
    truncate(len?: number): void;
    seekTo(position: number): void;
    stats(): Stats<number>;
    write(
      buf: Buffer,
      offset?: number,
      length?: number,
      position?: number,
    ): number;
    read(
      buf: Buffer | Uint8Array,
      offset?: number,
      length?: number,
      position?: number,
    ): number;
    chmod(perm: number): void;
    chown(uid: number, gid: number): void;
  }
  declare type TStatNumber = number;
  /**
   * Statistics about a file/directory, like `fs.Stats`.
   */
  declare class Stats<T = TStatNumber> {
    static build(node: Node, bigint: false): Stats<number>;
    // static build(node: Node, bigint: true): Stats<bigint>;
    static build(node: Node, bigint?: boolean): Stats<TStatNumber>;
    uid: T;
    gid: T;
    rdev: T;
    blksize: T;
    ino: T;
    size: T;
    blocks: T;
    atime: Date;
    mtime: Date;
    ctime: Date;
    birthtime: Date;
    atimeMs: T;
    mtimeMs: T;
    ctimeMs: T;
    birthtimeMs: T;
    dev: T;
    mode: T;
    nlink: T;
    isDirectory(): boolean;
    isFile(): boolean;
    isBlockDevice(): boolean;
    isCharacterDevice(): boolean;
    isSymbolicLink(): boolean;
    isFIFO(): boolean;
    isSocket(): boolean;
  }
  declare class Dirent {
    static build(link: Link, encoding: TEncodingExtended | void): Dirent;
    name: TDataOut;
    isDirectory(): boolean;
    isFile(): boolean;
    isBlockDevice(): boolean;
    isCharacterDevice(): boolean;
    isSymbolicLink(): boolean;
    isFIFO(): boolean;
    isSocket(): boolean;
  }

  declare class Volume {
    static fromJSON(json: DirectoryJSON, cwd?: string): Volume;
    static fromNestedJSON(json: NestedDirectoryJSON, cwd?: string): Volume;
    /**
     * Global file descriptor counter. UNIX file descriptors start from 0 and go sequentially
     * up, so here, in order not to conflict with them, we choose some big number and descrease
     * the file descriptor of every new opened file.
     * @type {number}
     * @todo This should not be static, right?
     */
    static fd: number;
    root: Link;
    ino: number;
    inodes: {
      [ino: number]: Node,
    };
    releasedInos: number[];
    fds: {
      [fd: number]: File,
    };
    releasedFds: number[];
    maxFiles: number;
    openFiles: number;
    // get promises(): import("./promises").IPromisesAPI;
    constructor(props?: {}): Volume;
    createLink(): Link;
    createLink(
      parent: Link,
      name: string,
      isDirectory?: boolean,
      perm?: number,
    ): Link;
    deleteLink(link: Link): boolean;
    createNode(isDirectory?: boolean, perm?: number): Node;
    genRndStr(): any;
    getLink(steps: string[]): Link | null;
    getLinkOrThrow(filename: string, funcName?: string): Link;
    getResolvedLink(filenameOrSteps: string | string[]): Link | null;
    getResolvedLinkOrThrow(filename: string, funcName?: string): Link;
    resolveSymlinks(link: Link): Link | null;
    /**
     * @todo This is not used anymore. Remove.
     */
    toJSON(
      paths?: PathLike | PathLike[],
      json?: {},
      isRelative?: boolean,
    ): DirectoryJSON;
    fromJSON(json: DirectoryJSON, cwd?: string): void;
    fromNestedJSON(json: NestedDirectoryJSON, cwd?: string): void;
    reset(): void;
    mountSync(mountpoint: string, json: DirectoryJSON): void;
    openSync(path: PathLike, flags: TFlags, mode?: TMode): number;
    open(
      path: PathLike,
      flags: TFlags,
      /* ... */ callback: TCallback<number>,
    ): any;
    open(
      path: PathLike,
      flags: TFlags,
      mode: TMode,
      callback: TCallback<number>,
    ): any;
    closeSync(fd: number): void;
    close(fd: number, callback: TCallback<void>): void;
    readSync(
      fd: number,
      buffer: Buffer | Uint8Array,
      offset: number,
      length: number,
      position: number,
    ): number;
    read(
      fd: number,
      buffer: Buffer | Uint8Array,
      offset: number,
      length: number,
      position: number,
      callback: (
        err?: Error | null,
        bytesRead?: number,
        buffer?: Buffer | Uint8Array,
      ) => void,
    ): void;
    readFileSync(file: TFileId, options?: IReadFileOptions | string): TDataOut;
    readFile(id: TFileId, callback: TCallback<TDataOut>): any;
    readFile(
      id: TFileId,
      options: IReadFileOptions | string,
      callback: TCallback<TDataOut>,
    ): any;
    writeSync(
      fd: number,
      buffer: Buffer | Uint8Array,
      offset?: number,
      length?: number,
      position?: number,
    ): number;
    writeSync(
      fd: number,
      str: string,
      position?: number,
      encoding?: buffer$Encoding,
    ): number;
    write(
      fd: number,
      buffer: Buffer | Uint8Array,
      callback: (...args: any[]) => void,
    ): any;
    write(
      fd: number,
      buffer: Buffer | Uint8Array,
      offset: number,
      callback: (...args: any[]) => void,
    ): any;
    write(
      fd: number,
      buffer: Buffer | Uint8Array,
      offset: number,
      length: number,
      callback: (...args: any[]) => void,
    ): any;
    write(
      fd: number,
      buffer: Buffer | Uint8Array,
      offset: number,
      length: number,
      position: number,
      callback: (...args: any[]) => void,
    ): any;
    write(fd: number, str: string, callback: (...args: any[]) => void): any;
    write(
      fd: number,
      str: string,
      position: number,
      callback: (...args: any[]) => void,
    ): any;
    write(
      fd: number,
      str: string,
      position: number,
      encoding: buffer$Encoding,
      callback: (...args: any[]) => void,
    ): any;
    writeFileSync(id: TFileId, data: TData, options?: IWriteFileOptions): void;
    writeFile(id: TFileId, data: TData, callback: TCallback<void>): any;
    writeFile(
      id: TFileId,
      data: TData,
      options: IWriteFileOptions | string,
      callback: TCallback<void>,
    ): any;
    copyFileSync(src: PathLike, dest: PathLike, flags?: TFlagsCopy): void;
    copyFile(src: PathLike, dest: PathLike, callback: TCallback<void>): any;
    copyFile(
      src: PathLike,
      dest: PathLike,
      flags: TFlagsCopy,
      callback: TCallback<void>,
    ): any;
    linkSync(existingPath: PathLike, newPath: PathLike): void;
    link(
      existingPath: PathLike,
      newPath: PathLike,
      callback: TCallback<void>,
    ): void;
    unlinkSync(path: PathLike): void;
    unlink(path: PathLike, callback: TCallback<void>): void;
    symlinkSync(target: PathLike, path: PathLike, type?: symlink$Type): void;
    symlink(target: PathLike, path: PathLike, callback: TCallback<void>): any;
    symlink(
      target: PathLike,
      path: PathLike,
      type: symlink$Type,
      callback: TCallback<void>,
    ): any;
    realpathSync(path: PathLike, options?: IRealpathOptions | string): TDataOut;
    realpath(path: PathLike, callback: TCallback<TDataOut>): any;
    realpath(
      path: PathLike,
      options: IRealpathOptions | string,
      callback: TCallback<TDataOut>,
    ): any;
    lstatSync(path: PathLike): Stats<number>;
    lstatSync(
      path: PathLike,
      options: {
        throwIfNoEntry?: true | void,
      },
    ): Stats<number>;
    lstatSync(
      path: PathLike,
      options: {
        bigint: false,
        throwIfNoEntry?: true | void,
      },
    ): Stats<number>;
    // lstatSync(
    //   path: PathLike,
    //   options: {
    //     bigint: true,
    //     throwIfNoEntry?: true | void,
    //   },
    // ): Stats<bigint>;
    lstatSync(
      path: PathLike,
      options: {
        throwIfNoEntry: false,
      },
    ): Stats<number> | void;
    lstatSync(
      path: PathLike,
      options: {
        bigint: false,
        throwIfNoEntry: false,
      },
    ): Stats<number> | void;
    // lstatSync(
    //   path: PathLike,
    //   options: {
    //     bigint: true,
    //     throwIfNoEntry: false,
    //   },
    // ): Stats<bigint> | void;
    lstat(path: PathLike, callback: TCallback<Stats<>>): void;
    lstat(
      path: PathLike,
      options: IStatOptions,
      callback: TCallback<Stats<>>,
    ): void;
    statSync(path: PathLike): Stats<number>;
    statSync(
      path: PathLike,
      options: {
        throwIfNoEntry?: true,
      },
    ): Stats<number>;
    statSync(
      path: PathLike,
      options: {
        throwIfNoEntry: false,
      },
    ): Stats<number> | void;
    statSync(
      path: PathLike,
      options: {
        bigint: false,
        throwIfNoEntry?: true,
      },
    ): Stats<number>;
    // statSync(
    //   path: PathLike,
    //   options: {
    //     bigint: true,
    //     throwIfNoEntry?: true,
    //   },
    // ): Stats<bigint>;
    statSync(
      path: PathLike,
      options: {
        bigint: false,
        throwIfNoEntry: false,
      },
    ): Stats<number> | void;
    // statSync(
    //   path: PathLike,
    //   options: {
    //     bigint: true,
    //     throwIfNoEntry: false,
    //   },
    // ): Stats<bigint> | void;
    stat(path: PathLike, callback: TCallback<Stats<>>): void;
    stat(
      path: PathLike,
      options: IStatOptions,
      callback: TCallback<Stats<>>,
    ): void;
    fstatSync(fd: number): Stats<number>;
    fstatSync(
      fd: number,
      options: {
        bigint: false,
      },
    ): Stats<number>;
    // fstatSync(
    //   fd: number,
    //   options: {
    //     bigint: true,
    //   },
    // ): Stats<bigint>;
    fstat(fd: number, callback: TCallback<Stats<>>): void;
    fstat(
      fd: number,
      options: IFStatOptions,
      callback: TCallback<Stats<>>,
    ): void;
    renameSync(oldPath: PathLike, newPath: PathLike): void;
    rename(
      oldPath: PathLike,
      newPath: PathLike,
      callback: TCallback<void>,
    ): void;
    existsSync(path: PathLike): boolean;
    exists(path: PathLike, callback: (exists: boolean) => void): void;
    accessSync(path: PathLike, mode?: number): void;
    access(path: PathLike, callback: TCallback<void>): any;
    access(path: PathLike, mode: number, callback: TCallback<void>): any;
    appendFileSync(
      id: TFileId,
      data: TData,
      options?: IAppendFileOptions | string,
    ): void;
    appendFile(id: TFileId, data: TData, callback: TCallback<void>): any;
    appendFile(
      id: TFileId,
      data: TData,
      options: IAppendFileOptions | string,
      callback: TCallback<void>,
    ): any;
    readdirSync(
      path: PathLike,
      options?: IReaddirOptions | string,
    ): TDataOut[] | Dirent[];
    readdir(path: PathLike, callback: TCallback<TDataOut[] | Dirent[]>): any;
    readdir(
      path: PathLike,
      options: IReaddirOptions | string,
      callback: TCallback<TDataOut[] | Dirent[]>,
    ): any;
    readlinkSync(path: PathLike, options?: IOptions): TDataOut;
    readlink(path: PathLike, callback: TCallback<TDataOut>): any;
    readlink(
      path: PathLike,
      options: IOptions,
      callback: TCallback<TDataOut>,
    ): any;
    fsyncSync(fd: number): void;
    fsync(fd: number, callback: TCallback<void>): void;
    fdatasyncSync(fd: number): void;
    fdatasync(fd: number, callback: TCallback<void>): void;
    ftruncateSync(fd: number, len?: number): void;
    ftruncate(fd: number, callback: TCallback<void>): any;
    ftruncate(fd: number, len: number, callback: TCallback<void>): any;
    truncateSync(id: TFileId, len?: number): void;
    truncate(id: TFileId, callback: TCallback<void>): any;
    truncate(id: TFileId, len: number, callback: TCallback<void>): any;
    futimesSync(fd: number, atime: TTime, mtime: TTime): void;
    futimes(
      fd: number,
      atime: TTime,
      mtime: TTime,
      callback: TCallback<void>,
    ): void;
    utimesSync(path: PathLike, atime: TTime, mtime: TTime): void;
    utimes(
      path: PathLike,
      atime: TTime,
      mtime: TTime,
      callback: TCallback<void>,
    ): void;
    /**
     * Creates directory tree recursively.
     * @param filename
     * @param modeNum
     */
    mkdirSync(path: PathLike, options?: TMode | IMkdirOptions): void;
    mkdir(path: PathLike, callback: TCallback<void>): any;
    mkdir(
      path: PathLike,
      mode: TMode | IMkdirOptions,
      callback: TCallback<void>,
    ): any;
    mkdirpSync(path: PathLike, mode?: TMode): void;
    mkdirp(path: PathLike, callback: TCallback<void>): any;
    mkdirp(path: PathLike, mode: TMode, callback: TCallback<void>): any;
    mkdtempSync(prefix: string, options?: IOptions): TDataOut;
    mkdtemp(prefix: string, callback: TCallback<void>): any;
    mkdtemp(prefix: string, options: IOptions, callback: TCallback<void>): any;
    rmdirSync(path: PathLike, options?: IRmdirOptions): void;
    rmdir(path: PathLike, callback: TCallback<void>): any;
    rmdir(
      path: PathLike,
      options: IRmdirOptions,
      callback: TCallback<void>,
    ): any;
    rmSync(path: PathLike, options?: IRmOptions): void;
    rm(path: PathLike, callback: TCallback<void>): void;
    rm(path: PathLike, options: IRmOptions, callback: TCallback<void>): void;
    fchmodSync(fd: number, mode: TMode): void;
    fchmod(fd: number, mode: TMode, callback: TCallback<void>): void;
    chmodSync(path: PathLike, mode: TMode): void;
    chmod(path: PathLike, mode: TMode, callback: TCallback<void>): void;
    lchmodSync(path: PathLike, mode: TMode): void;
    lchmod(path: PathLike, mode: TMode, callback: TCallback<void>): void;
    fchownSync(fd: number, uid: number, gid: number): void;
    fchown(
      fd: number,
      uid: number,
      gid: number,
      callback: TCallback<void>,
    ): void;
    chownSync(path: PathLike, uid: number, gid: number): void;
    chown(
      path: PathLike,
      uid: number,
      gid: number,
      callback: TCallback<void>,
    ): void;
    lchownSync(path: PathLike, uid: number, gid: number): void;
    lchown(
      path: PathLike,
      uid: number,
      gid: number,
      callback: TCallback<void>,
    ): void;
    // watchFile(
    //   path: PathLike,
    //   listener: (curr: Stats<>, prev: Stats<>) => void,
    // ): StatWatcher;
    // watchFile(
    //   path: PathLike,
    //   options: IWatchFileOptions,
    //   listener: (curr: Stats<>, prev: Stats<>) => void,
    // ): StatWatcher;
    unwatchFile(
      path: PathLike,
      listener?: (curr: Stats<>, prev: Stats<>) => void,
    ): void;
    createReadStream(
      path: PathLike,
      options?: IReadStreamOptions | string,
    ): IReadStream;
    createWriteStream(
      path: PathLike,
      options?: IWriteStreamOptions | string,
    ): IWriteStream;
    // watch(
    //   path: PathLike,
    //   options?: IWatchOptions | string,
    //   listener?: (eventType: string, filename: string) => void,
    // ): FSWatcher;
  }

  declare class IFs extends Volume {
    // constants: typeof constants;
    // Stats: new (...args: any[]) => Stats;
    // Dirent: new (...args: any[]) => Dirent;
    // StatWatcher: new () => StatWatcher;
    // FSWatcher: new () => FSWatcher;
    // ReadStream: new (...args: any[]) => IReadStream;
    // WriteStream: new (...args: any[]) => IWriteStream;
    promises: mixed;
  }

  declare export var fs: IFs;
  declare export var vol: Volume;
}
