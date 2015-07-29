/**
 * Copyright (c) 2014, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 */

declare class Buffer {
  constructor(value: Array<number> | number | string | Buffer, encoding?: string): void;
  [i: number]: number;
  length: number;
  write(string: string, offset?: number, length?: number, encoding?: string): void;
  copy(targetBuffer: Buffer, targetStart?: number, sourceStart?: number, sourceEnd?: number): number;
  equals(otherBuffer: Buffer): boolean;
  compare(otherBuffer: Buffer): number;
  slice(start?: number, end?: number): Buffer;
  fill(value: string | number, offset?: number, end?: number): void;
  inspect(): string;
  toString(encoding?: string, start?: number, end?: number): string;
  toJSON(): Array<number>;

  readUInt8(offset: number, noAssert?: boolean): number;
  readUInt16LE(offset: number, noAssert?: boolean): number;
  readUInt16BE(offset: number, noAssert?: boolean): number;
  readUInt32LE(offset: number, noAssert?: boolean): number;
  readUInt32BE(offset: number, noAssert?: boolean): number;
  readInt8(offset: number, noAssert?: boolean): number;
  readInt16LE(offset: number, noAssert?: boolean): number;
  readInt16BE(offset: number, noAssert?: boolean): number;
  readInt32LE(offset: number, noAssert?: boolean): number;
  readInt32BE(offset: number, noAssert?: boolean): number;
  readFloatLE(offset: number, noAssert?: boolean): number;
  readFloatBE(offset: number, noAssert?: boolean): number;
  readDoubleLE(offset: number, noAssert?: boolean): number;
  readDoubleBE(offset: number, noAssert?: boolean): number;
  writeUInt8(value: number, offset: number, noAssert?: boolean): number;
  writeUInt16LE(value: number, offset: number, noAssert?: boolean): number;
  writeUInt16BE(value: number, offset: number, noAssert?: boolean): number;
  writeUInt32LE(value: number, offset: number, noAssert?: boolean): number;
  writeUInt32BE(value: number, offset: number, noAssert?: boolean): number;
  writeInt8(value: number, offset: number, noAssert?: boolean): number;
  writeInt16LE(value: number, offset: number, noAssert?: boolean): number;
  writeInt16BE(value: number, offset: number, noAssert?: boolean): number;
  writeInt32LE(value: number, offset: number, noAssert?: boolean): number;
  writeInt32BE(value: number, offset: number, noAssert?: boolean): number;
  writeFloatLE(value: number, offset: number, noAssert?: boolean): number;
  writeFloatBE(value: number, offset: number, noAssert?: boolean): number;
  writeDoubleLE(value: number, offset: number, noAssert?: boolean): number;
  writeDoubleBE(value: number, offset: number, noAssert?: boolean): number;

  static isEncoding(encoding: string): boolean;
  static isBuffer(obj: any): boolean;
  static byteLength(string: string, encoding?: string): number;
  static concat(list: Array<Buffer>, totalLength?: number): Buffer;
  static compare(buf1: Buffer, buf2: Buffer): number;
}

declare module "buffer" {
  declare var INSPECT_MAX_BYTES: number;
}

type child_process$execOpts = {
  cwd?: string;
  env?: Object;
  encoding?: string;
  shell?: string;
  timeout?: number;
  maxBuffer?: number;
  killSignal?: string;
  uid?: number;
  gid?: number;
};

type child_process$execCallback = (error: ?Error, stdout: Buffer, stderr: Buffer) => void;

type child_process$execSyncOpts = {
  cwd?: string;
  input?: string | Buffer;
  stdio?: Array<any>;
  env?: Object;
  uid?: number;
  gid?: number;
  timeout?: number;
  killSignal?: string;
  maxBuffer?: number;
  encoding?: string;
};

type child_process$execFileOpts = {
  cwd?: string;
  env?: Object;
  encoding?: string;
  timeout?: number;
  maxBuffer?: number;
  killSignal?: string;
  uid?: number;
  gid?: number;
};

type child_process$execFileCallback = (error: ?Error, stdout: Buffer, stderr: Buffer) => void;

type child_process$forkOpts = {
  cwd?: string;
  env?: Object;
  execPath?: string;
  execArgv?: Array<string>;
  silent?: boolean;
  uid?: number;
  gid?: number;
};

type child_process$Handle = any; // TODO

type child_process$spawnOpts = {
  cwd?: string;
  env?: Object;
  stdio?: string | Array<any>;
  detached?: boolean;
  uid?: number;
  gid?: number;
};

type child_process$spawnRet = {
  pid: number;
  output: Array<any>;
  stdout: Buffer | string;
  stderr: Buffer | string;
  status: number;
  signal: string;
  error: Error;
};

declare class child_process$ChildProcess extends events$EventEmitter {
  stdin: stream$Writable;
  stdout: stream$Readable;
  stderr: stream$Readable;
  pid: number;
  connected: boolean;

  disconnect(): void;
  kill(signal?: string): void;
  send(message: Object, sendHandle?: child_process$Handle): void;
}

declare module "child_process" {
  declare function exec(
    command: string,
    optionsOrCallback?: child_process$execOpts | child_process$execCallback,
    callback?: child_process$execCallback
  ): child_process$ChildProcess;

  declare function execSync(
    command: string,
    options?: child_process$execSyncOpts
  ): Buffer | string;

  declare function execFile(
    file: string,
    argsOrOptionsOrCallback?:
      Array<string> | child_process$execFileOpts | child_process$execFileCallback,
    optionsOrCallback?: child_process$execFileOpts | child_process$execFileCallback,
    callback?: child_process$execFileCallback
  ): child_process$ChildProcess;

  declare function execFileSync(
    command: string,
    argsOrOptions?: Array<string> | child_process$execFileOpts,
    options?: child_process$execFileOpts
  ): Buffer | string;

  declare function fork(
    modulePath: string,
    argsOrOptions?: Array<string> | child_process$forkOpts,
    options?: child_process$forkOpts
  ): child_process$ChildProcess;

  declare function spawn(
    command: string,
    argsOrOptions?: Array<string> | child_process$spawnOpts,
    options?: child_process$spawnOpts
  ): child_process$ChildProcess;

  declare function spawnSync(
    command: string,
    argsOrOptions?: Array<string> | child_process$spawnOpts,
    options?: child_process$spawnOpts
  ): child_process$spawnRet;
}

type cluster$Worker = {
  id: string;
  process: child_process$ChildProcess;
  suicide: boolean;

  disconnect(): void;
  kill(signal?: string): void;
  send(message: Object, sendHandle?: child_process$Handle): void;
}

declare module "cluster" {
  declare var isMaster: boolean;
  declare var isWorker: boolean;
  declare var settings: {
    args: Array<string>;
    exec: string;
    execArgv: Array<string>;
    silent: boolean;
  };
  declare var worker: cluster$Worker;
  declare var workers: Object;

  declare function disconnect(callback?: () => void): void;
  declare function fork(env?: Object): cluster$Worker;
  declare function setupMaster(settings?: Object): void;
}

type crypto$createCredentialsDetails = any; // TODO

type crypto$Cipher = {
  final(output_encoding?: string): any;
  setAutoPadding(auto_padding?: boolean): void;
  update(data: any, input_encoding?: string, output_encoding?: string): any;
}

type crypto$Credentials = {
  // TODO
}

type crypto$DiffieHellman = {
  computeSecret(
    other_public_key: string,
    input_encoding?: string,
    output_encoding?: string
  ): any;
  generateKeys(encoding?: string): any;
  getGenerator(encoding?: string): any;
  getPrime(encoding?: string): any;
  getPrivateKey(encoding?: string): any;
  getPublicKey(encoding?: string): any;
  setPrivateKey(private_key: any, encoding?: string): void;
  setPublicKey(public_key: any, encoding?: string): void;
}

type crypto$Decipher = {
  update(data: any, input_encoding?: string, output_encoding?: string): any;
  final(output_encoding?: string): any;
  setAutoPadding(auto_padding?: boolean): void;
}

type crypto$Hash = {
  update(data: any, input_encoding?: string): void;
  digest(encoding?: string): any;
}

type crypto$Hmac = {
  update(data: any): void;
  digest(encoding?: string): any;
}

type crypto$Sign = {
  sign(private_key: string, output_format?: string): any;
  update(data: any): void;
}

type crypto$Verify = {
  update(data: any): void;
  verify(object: Object, signature: crypto$Sign, signature_format?: string): boolean;
}

declare module "crypto" {
  declare var DEFAULT_ENCODING: string;

  declare function createCipher(algorithm: string, password: string | Buffer): crypto$Cipher;
  declare function createCipheriv(
    algorithm: string,
    key: string | Buffer,
    iv: string | Buffer
  ): crypto$Cipher;
  declare function createCredentials(
    details?: crypto$createCredentialsDetails
  ): crypto$Credentials
  declare function createDecipher(algorithm: string, password: string | Buffer): crypto$Decipher;
  declare function createDecipheriv(
    algorithm: string,
    key: string | Buffer,
    iv: string | Buffer
  ): crypto$Decipher;
  declare function createDiffieHellman(prime_length: number): crypto$DiffieHellman;
  declare function createDiffieHellman(prime: number, encoding?: string): crypto$DiffieHellman;
  declare function createHash(algorithm: string): crypto$Hash;
  declare function createHmac(algorithm: string, key: string): crypto$Hmac;
  declare function creatSign(algorithm: string): crypto$Sign;
  declare function createVerify(algorithm: string): crypto$Verify;
  declare function getCiphers(): Array<string>;
  declare function getDiffieHellman(group_name: string): crypto$DiffieHellman;
  declare function getHashes(): Array<string>;
  declare function pbkdf2(
    password: string,
    salt: string,
    iterations: number,
    keylen: number,
    callback: (err: ?Error, derivedKey: string) => void
  ): void;
  declare function pbkdf2Sync(
    password: string,
    salt: string,
    iterations: number,
    keylen: number
  ): string;
  declare function pseudoRandomBytes(
    size: number,
    callback: (err: ?Error, buffer: Buffer) => void
  ): void;
  declare function randomBytes(
    size: number,
    callback?: (err: ?Error, buffer: Buffer) => void
  ): void;
}

type dgram$Socket = {
  addMembership(multicastAddress: string, multicastInterface?: string): void;
  address(): {address: string; family: string; port: number};
  bind(port: number, address?: string, callback?: () => void): void;
  close(): void;
  dropMembership(multicastAddress: string, multicastInterface?: string): void;
  ref(): void;
  send(
    buf: Buffer,
    offset: number,
    length: number,
    port: number,
    address: string,
    callback?: (err: ?Error, bytes: any) => void
  ): void;
  setBroadcast(flag: boolean): void;
  setMulticastLoopback(flag: boolean): void;
  setMulticastTTL(ttl: number): void;
  setTTL(ttl: number): void;
  unref(): void;
};

declare module "dgram" {
  declare function createSocket(type: string, callback?: () => void): dgram$Socket;
}

declare module "dns" {
  declare var ADDRGETNETWORKPARAMS: string;
  declare var BADFAMILY: string;
  declare var BADFLAGS: string;
  declare var BADHINTS: string;
  declare var BADQUERY: string;
  declare var BADNAME: string;
  declare var BADRESP: string;
  declare var BADSTR: string;
  declare var CANCELLED: string;
  declare var CONNREFUSED: string;
  declare var DESTRUCTION: string;
  declare var EOF: string;
  declare var FILE: string;
  declare var FORMER: string;
  declare var LOADIPHLPAPI: string;
  declare var NODATA: string;
  declare var NOMEM: string;
  declare var NONAME: string;
  declare var NOTFOUND: string;
  declare var NOTIMP: string;
  declare var NOTINITIALIZED: string;
  declare var REFUSED: string;
  declare var SERVFAIL: string;
  declare var TIMEOUT: string;

  declare function lookup(
    domain: string,
    family?: ?number,
    callback?: (err: ?Error, address: string, family: number) => void
  ): void;

  declare function resolve(
    domain: string,
    rrtype?: string,
    callback?: (err: ?Error, addresses: Array<any>) => void
  ): void;

  declare function resolve4(
    domain: string,
    callback: (err: ?Error, addresses: Array<any>) => void
  ): void;

  declare function resolve6(
    domain: string,
    callback: (err: ?Error, addresses: Array<any>) => void
  ): void;

  declare function resolveCname(
    domain: string,
    callback: (err: ?Error, addresses: Array<any>) => void
  ): void;

  declare function resolveMx(
    domain: string,
    callback: (err: ?Error, addresses: Array<any>) => void
  ): void;

  declare function resolveNs(
    domain: string,
    callback: (err: ?Error, addresses: Array<any>) => void
  ): void;

  declare function resolveSrv(
    domain: string,
    callback: (err: ?Error, addresses: Array<any>) => void
  ): void;

  declare function resolveTxt(
    domain: string,
    callback: (err: ?Error, addresses: Array<any>) => void
  ): void;

  declare function reverse(
    ip: string,
    callback: (err: ?Error, domains: Array<any>) => void
  ): void;
}

type domain$Domain = {
  members: Array<any>;

  add(emitter: events$EventEmitter): void;
  bind(callback: Function): Function;
  dispose(): void;
  enter(): void;
  exit(): void;
  intercept(callback: Function): Function;
  remove(emitter: events$EventEmitter): void;
  run(fn: Function): void;
};

declare module "domain" {
  declare function create(): domain$Domain;
}

// TODO: This is copypasta of the EventEmitter class signature exported from the
//       `events` module. The only reason this exists is because other module
//       interface definitions need to reference this type structure -- but
//       referencing type structures defined in other modules isn't possible at
//       the time of this writing.
declare class events$EventEmitter {
  static listenerCount(emitter: events$EventEmitter, event: string): number;

  addListener(event: string, listener: Function): events$EventEmitter;
  emit(event: string, ...args:Array<any>): boolean;
  listeners(event: string): Array<Function>;
  on(event: string, listener: Function): events$EventEmitter;
  once(event: string, listener: Function): events$EventEmitter;
  removeAllListeners(event?: string): events$EventEmitter;
  removeListener(event: string, listener: Function): events$EventEmitter;
  setMaxListeners(n: number): void;
}

declare module "events" {
  // TODO: See the comment above the events$EventEmitter declaration
  declare class EventEmitter extends events$EventEmitter {}
}

declare module "fs" {
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

  declare function rename(oldPath: string, newPath: string, callback?: (err: ?Error) => void): void;
  declare function renameSync(oldPath: string, newPath: string): void;
  declare function ftruncate(fd: number, len: number, callback?: (err: ?Error) => void): void;
  declare function ftruncateSync(fd: number, len: number): void;
  declare function truncate(path: string, len: number, callback?: (err: ?Error) => void): void;
  declare function truncateSync(path: string, len: number): void;
  declare function chown(path: string, uid: number, gid: number, callback?: (err: ?Error) => void): void;
  declare function chownSync(path: string, uid: number, gid: number): void;
  declare function fchown(fd: number, uid: number, gid: number, callback?: (err: ?Error) => void): void;
  declare function fchownSync(fd: number, uid: number, gid: number): void;
  declare function lchown(path: string, uid: number, gid: number, callback?: (err: ?Error) => void): void;
  declare function lchownSync(path: string, uid: number, gid: number): void;
  declare function chmod(path: string, mode: number | string, callback?: (err: ?Error) => void): void;
  declare function chmodSync(path: string, mode: number | string): void;
  declare function fchmod(fd: number, mode: number | string, callback?: (err: ?Error) => void): void;
  declare function fchmodSync(fd: number, mode: number | string): void;
  declare function lchmod(path: string, mode: number | string, callback?: (err: ?Error) => void): void;
  declare function lchmodSync(path: string, mode: number | string): void;
  declare function stat(path: string, callback?: (err: ?Error, stats: Stats) => any): void;
  declare function statSync(path: string): Stats;
  declare function fstat(fd: number, callback?: (err: ?Error, stats: Stats) => any): void;
  declare function fstatSync(fd: number): Stats;
  declare function lstat(path: string, callback?: (err: ?Error, stats: Stats) => any): void;
  declare function lstatSync(path: string): Stats;
  declare function link(srcpath: string, dstpath: string, callback?: (err: ?Error) => void): void;
  declare function linkSync(srcpath: string, dstpath: string): void;
  declare function symlink(srcpath: string, dtspath: string, type?: string, callback?: (err: ?Error) => void): void;
  declare function symlinkSync(srcpath: string, dstpath: string, type: string): void;
  declare function readlink(path: string, callback: (err: ?Error, linkString: string) => void): void;
  declare function readlinkSync(path: string): string;
  declare function realpath(path: string, cache?: Object, callback?: (err: ?Error, resolvedPath: string) => void): void;
  declare function realpathSync(path: string, cache?: Object): string;
  declare function unlink(path: string, callback?: (err: ?Error) => void): void;
  declare function unlinkSync(path: string): void;
  declare function rmdir(path: string, callback?: (err: ?Error) => void): void;
  declare function rmdirSync(path: string): void;
  declare function mkdir(path: string, mode?: number, callback?: (err: ?Error) => void): void;
  declare function mkdirSync(path: string, mode?: number): void;
  declare function readdir(path: string, callback?: (err: ?Error, files: Array<string>) => void): void;
  declare function readdirSync(path: string): Array<string>;
  declare function close(fd: number, callback?: (err: ?Error) => void): void;
  declare function closeSync(fd: number): void;
  declare function open(path: string, flags: string, mode?: number, callback?: (err: ?Error, fd: number) => void): void;
  declare function openSync(path: string, flags: string, mode?: number): number;
  declare function utimes(path: string, atime: number, mtime: number, callback?: (err: ?Error) => void): void;
  declare function utimesSync(path: string, atime: number, mtime: number): void;
  declare function futimes(fd: number, atime: number, mtime: number, callback?: (err: ?Error) => void): void;
  declare function futimesSync(fd: number, atime: number, mtime: number): void;
  declare function fsync(fd: number, callback?: (err: ?Error) => void): void;
  declare function fsyncSync(fd: number): void;
  declare var write: (fd: number, buffer: Buffer, offset: number, length: number, position?: mixed, callback?: (err: ?Error, write: number, str: string) => void) => void
                   | (fd: number, data: mixed, position?: mixed, encoding?: string, callback?: (err: ?Error, write: number, str: string) => void) => void;
  declare var writeSync: (fd: number, buffer: Buffer, offset: number, length: number, position?: number) => number
                       | (fd: number, data: mixed, position?: mixed, encoding?: string) => number;
  declare function read(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: ?number,
    callback?: (err: ?Error, bytesRead: number, buffer: Buffer) => void
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
    callback: (err: ?Error, data: Buffer) => void
  ): void;
  declare function readFile(
    filename: string,
    encoding: string,
    callback: (err: ?Error, data: string) => void
  ): void;
  declare function readFile(
    filename: string,
    options: { encoding: string; flag?: string },
    callback: (err: ?Error, data: string) => void
  ): void;
  declare function readFile(
    filename: string,
    options: { flag?: string },
    callback: (err: ?Error, data: Buffer) => void
  ): void;
  declare function readFileSync(filename: string): Buffer;
  declare function readFileSync(filename: string, encoding: string): string;
  declare function readFileSync(filename: string, options: { encoding: string, flag?: string }): string;
  declare function readFileSync(filename: string, options: { flag?: string }): Buffer;
  declare function writeFile(
    filename: string,
    data: Buffer | string,
    options?: Object | string,
    callback?: (err: ?Error) => void
  ): void;
  declare function writeFileSync(
    filename: string,
    data: Buffer | string,
    options?: Object | string
  ): void;
  declare function appendFile(filename: string, data: string | Buffer, options?: Object, callback?: (err: ?Error) => void): void;
  declare function appendFileSync(filename: string, data: string | Buffer, options?: Object): void;
  declare function watchFile(filename: string, options?: Object, listener?: (curr: Stats, prev: Stats) => void): void;
  declare function unwatchFile(filename: string, listener?: (curr: Stats, prev: Stats) => void): void;
  declare function watch(filename: string, options?: Object, listener?: (event: string, filename: string) => void): FSWatcher;
  declare function exists(path: string, callback?: (exists: boolean) => void): void;
  declare function existsSync(path: string): boolean;
  declare function access(path: string, mode?: any, callback?: (err: ?Error) => void): void;
  declare function accessSync(path: string, mode?: any): void;
  declare function createReadStream(path: string, options?: Object): ReadStream;
  declare function createWriteStream(path: string, options?: Object): WriteStream;
}

declare module "http" {
  declare class Server extends events$EventEmitter {
    listen(port: number, hostname?: string, backlog?: number, callback?: Function): Server;
    listen(path: string, callback?: Function): Server;
    listen(handle: Object, callback?: Function): Server;
    close(callback?: Function): Server;
    address(): { port: number; fmaily: string; address: string; };
    maxHeadersCount: number;
  }

  declare class IncomingMessage extends stream$Readable {
    headers: Object;
    httpVersion: string;
    method: string;
    trailers: Object;
    setTimeout(msecs: number, callback: Function): void;
    socket: any;  // TODO net.Socket
    statusCode: number;
    url: String;
  }

  declare class OutgoingMessage extends stream$Stream {
    write(ins: string): void;
    end(): void;
  }

  declare class ClientRequest extends OutgoingMessage {
    // TODO
  }

  declare function createServer(listener: Function): Server;
  declare function request(
    options: Object | string,
    callback: (response: IncomingMessage) => void
  ): ClientRequest;
  declare function get(
    options: Object | string,
    callback: (response: IncomingMessage) => void
  ): ClientRequest;
}

declare module "https" {
  // TODO
}

declare module "net" {
  declare function isIP(input: string): number;
  declare function isIPv4(input: string): boolean;
  declare function isIPv6(input: string): boolean;
}

type os$CPU = {
  model: string,
  speed: number,
  times: {
    idle: number,
    irq: number,
    nice: number,
    sys: number,
    user: number,
  }
};

type os$NetIFAddr = {
  address: string,
  family: string,
  internal: boolean,
  mac: string,
  netmask: string
};

declare module "os" {
  declare function arch(): "x64"|"arm"|"ia32";
  declare function cpus(): Array<os$CPU>;
  declare function endianness(): "BE"|"LE";
  declare function freemem(): number;
  declare function hostname(): string;
  declare function loadavg(): [number, number, number];
  declare function networkInterfaces(): {[ifName: string]: Array<os$NetIFAddr>};
  declare function platform(): string;
  declare function release(): string;
  declare function tmpdir(): string;
  declare function totalmem(): number;
  declare function type(): string;
  declare function uptime(): number;
  declare var EOL: string;
}

declare module "path" {
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
    root: string;
    dir: string;
    base: string;
    ext: string;
    name: string;
  };
  declare function format(pathObject: {
    root?: string;
    dir?: string;
    base?: string;
    ext?: string;
    name?: string;
  }): string;
  declare var posix: any;
  declare var win32: any;
}

declare module "punycode" {
  // TODO
}

declare module "querystring" {
  declare function stringify(
    obj: Object,
    separator?: string,
    equal?: string,
    options?: {
      encodeURIComponent?: (str: string) => string;
    }
  ): string;
  declare function parse(
    str: string,
    separator: ?string,
    equal: ?string,
    options?: {
      decodeURIComponent?: (str: string) => string;
      maxKeys?: number;
    }
  ): any;
  declare function escape(str: string): string;
  declare function unescape(str: string, decodeSpaces?: boolean): string;
}

declare module "readline" {
  // TODO
}

declare class stream$Stream extends events$EventEmitter {
  pipe(dest: any, options?: any): void;
}

declare class stream$Readable extends stream$Stream {
  setEncoding(): void;
  isPaused(): boolean;
  pause(): void;
  read(size?: number): ?(string | Buffer);
  resume(): void;
  unpipe(dest?: any): void;
  unshift(chunk: any): void;
  wrap(oldReadable: any): stream$Readable;
}

declare class stream$Writable extends stream$Stream {
  end(
    chunk?: string | Buffer,
    encodingOrCallback?: string | (data: any) => void,
    callback?: (data: any) => void
  ): void;
  setDefaultEncoding(encoding: string): boolean;
  write(
    chunk?: string | Buffer,
    encodingOrCallback?: string | (data: any) => void,
    callback?: (data: any) => void
  ): boolean;
}

declare class stream$Duplex extends stream$Stream {
  setEncoding(): void;
  isPaused(): boolean;
  pause(): void;
  read(size?: number): ?(string | Buffer);
  resume(): void;
  unpipe(dest?: any): void;
  unshift(chunk: any): void;
  wrap(oldReadable: any): stream$Readable;
  end(
    chunk?: string | Buffer,
    encodingOrCallback?: string | (data: any) => void,
    callback?: (data: any) => void
  ): void;
  setDefaultEncoding(encoding: string): boolean;
  write(
    chunk?: string | Buffer,
    encodingOrCallback?: string | (data: any) => void,
    callback?: (data: any) => void
  ): boolean;
}

declare module "stream" {
  declare class Stream extends stream$Stream {}
}

declare module "string_decoder" {
  // TODO
}

declare module "tls" {
  // TODO
}

declare module "url" {
  declare function parse(
    urlStr: string,
    parseQueryString?: boolean,
    slashesDenoteHost?: boolean
  ): {
    protocol: ?string;
    slashes: ?boolean;
    auth: ?string;
    host: ?string;
    port: ?string;
    hostname: ?string;
    hash: ?string;
    search: ?string;
    query: ?any; // null | string | Object
    pathname: ?string;
    path: ?string;
    href: string;
  };
  declare function format(urlObj: {
    href?: string;
    protocol?: string;
    slashes?: boolean;
    auth?: string;
    hostname?: string;
    port?: string | number;
    host?: string;
    pathname?: string;
    search?: string;
    query?: Object;
    hash?: string;
  }): string;
  declare function resolve(from: string, to: string): string;
}

declare module "util" {
  declare function debuglog(section: string): (data: any, ...args: any) => void;
  declare function format(format: string, ...placeholders: any): string;
  declare function log(string: string): void;
  declare function inspect(object: any, options?: {
    showHidden?: boolean;
    depth?: ?number;
    colors?: boolean;
    customInspect?: boolean;
  }): string;
  declare function isArray(object: any): boolean;
  declare function isRegExp(object: any): boolean;
  declare function isDate(object: any): boolean;
  declare function isError(object: any): boolean;
  declare function inherits(constructor: Function, superConstructor: Function): void;
  declare function deprecate(f: Function, string: string): Function;
}

declare module "vm" {
  // TODO
}

type zlib$options = {
  flush?: number;
  chunkSize?: number;
  windowBits?: number;
  level?: number;
  memLevel?: number;
  strategy?: number;
  dictionary?: Buffer;
};

type zlib$syncFn = (
  buffer: string | Buffer,
  options?: zlib$options
) => Buffer;

type zlib$asyncFn = (
  buffer: string | Buffer,
  options?: zlib$options,
  callback?: ((error: ?Error, result: any) => void)
) => void;

declare module "zlib" {
  declare var Z_NO_FLUSH: number;
  declare var Z_PARTIAL_FLUSH: number;
  declare var Z_SYNC_FLUSH: number;
  declare var Z_FULL_FLUSH: number;
  declare var Z_FINISH: number;
  declare var Z_BLOCK: number;
  declare var Z_TREES: number;
  declare var Z_OK: number;
  declare var Z_STREAM_END: number;
  declare var Z_NEED_DICT: number;
  declare var Z_ERRNO: number;
  declare var Z_STREAM_ERROR: number;
  declare var Z_DATA_ERROR: number;
  declare var Z_MEM_ERROR: number;
  declare var Z_BUF_ERROR: number;
  declare var Z_VERSION_ERROR: number;
  declare var Z_NO_COMPRESSION: number;
  declare var Z_BEST_SPEED: number;
  declare var Z_BEST_COMPRESSION: number;
  declare var Z_DEFAULT_COMPRESSION: number;
  declare var Z_FILTERED: number;
  declare var Z_HUFFMAN_ONLY: number;
  declare var Z_RLE: number;
  declare var Z_FIXED: number;
  declare var Z_DEFAULT_STRATEGY: number;
  declare var Z_BINARY: number;
  declare var Z_TEXT: number;
  declare var Z_ASCII: number;
  declare var Z_UNKNOWN: number;
  declare var Z_DEFLATED: number;
  declare var Z_NULL: number;
  declare var Z_DEFAULT_CHUNK: number;
  declare var Z_DEFAULT_LEVEL: number;
  declare var Z_DEFAULT_MEMLEVEL: number;
  declare var Z_DEFAULT_WINDOWBITS: number;
  declare var Z_MAX_CHUNK: number;
  declare var Z_MAX_LEVEL: number;
  declare var Z_MAX_MEMLEVEL: number;
  declare var Z_MAX_WINDOWBITS: number;
  declare var Z_MIN_CHUNK: number;
  declare var Z_MIN_LEVEL: number;
  declare var Z_MIN_MEMLEVEL: number;
  declare var Z_MIN_WINDOWBITS: number;
  declare var codes: {
    Z_OK: number,
    Z_STREAM_END: number,
    Z_NEED_DICT: number,
    Z_ERRNO: number,
    Z_STREAM_ERROR: number,
    Z_DATA_ERROR: number,
    Z_MEM_ERROR: number,
    Z_BUF_ERROR: number,
    Z_VERSION_ERROR: number
  };
  declare class Zlib extends stream$Duplex {
    // TODO
  }
  declare class Deflate extends Zlib {}
  declare class Inflate extends Zlib {}
  declare class Gzip extends Zlib {}
  declare class Gunzip extends Zlib {}
  declare class DeflateRaw extends Zlib {}
  declare class InflateRaw extends Zlib {}
  declare class Unzip extends Zlib {}
  declare function createDeflate(options?: zlib$options): Deflate;
  declare function createInflate(options?: zlib$options): Inflate;
  declare function createDeflateRaw(options?: zlib$options): DeflateRaw;
  declare function createInflateRaw(options?: zlib$options): InflateRaw;
  declare function createGzip(options?: zlib$options): Gzip;
  declare function createGunzip(options?: zlib$options): Gunzip;
  declare function createUnzip(options?: zlib$options): Unzip;
  declare var deflate: zlib$asyncFn;
  declare var deflateSync: zlib$syncFn;
  declare var gzip: zlib$asyncFn;
  declare var gzipSync: zlib$syncFn;
  declare var deflateRaw: zlib$asyncFn;
  declare var deflateRawSync: zlib$syncFn;
  declare var unzip: zlib$asyncFn;
  declare var unzipSync: zlib$syncFn;
  declare var inflate: zlib$asyncFn;
  declare var inflateSync: zlib$syncFn;
  declare var gunzip: zlib$asyncFn;
  declare var gunzipSync: zlib$syncFn;
  declare var inflateRaw: zlib$asyncFn;
  declare var inflateRawSync: zlib$syncFn;
}

declare module "assert" {
  declare var exports: {
    (value: any, message?: string): void;
    ok(value: any, message?: string): void;
    fail(actual: any, expected: any, message: string, operator: string): void;
    equal(actual: any, expected: any, message?: string): void;
    notEqual(actual: any, expected: any, message?: string): void;
    deepEqual(actual: any, expected: any, message?: string): void;
    notDeepEqual(actual: any, expected: any, message?: string): void;
    strictEqual(actual: any, expected: any, message?: string): void;
    notStrictEqual(actual: any, expected: any, message?: string): void;
    throws(
      block: Function,
      error?: Function | RegExp | (err: any) => boolean,
      message?: string
    ): void;
    doesNotThrow(block: Function, message?: string): void;
    ifError(value: any): void;
  }
}

/* globals: https://nodejs.org/api/globals.html */

declare var process: any;

declare var __filename: string;
declare var __dirname: string;

declare function setImmediate(callback: ((...args: Array<any>) => void), ...args: Array<any>): Object;
declare function clearImmediate(immediateObject: any): Object;
