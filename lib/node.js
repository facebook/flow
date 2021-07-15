/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

interface ErrnoError extends Error {
  address?: string;
  code?: string;
  dest?: string;
  errno?: string | number;
  info?: Object;
  path?: string;
  port?: number;
  syscall?: string;
}

type buffer$NonBufferEncoding =
  'hex' | 'HEX' |
  'utf8' | 'UTF8' | 'utf-8' | 'UTF-8' |
  'ascii' | 'ASCII' |
  'binary' | 'BINARY' |
  'base64' | 'BASE64' |
  'ucs2' | 'UCS2' | 'ucs-2' | 'UCS-2' |
  'utf16le' | 'UTF16LE' | 'utf-16le' | 'UTF-16LE' | 'latin1';
type buffer$Encoding = buffer$NonBufferEncoding | 'buffer'
type buffer$ToJSONRet = {
  type: string,
  data: Array<number>,
  ...
}

declare class Buffer extends Uint8Array {
  constructor(
    value: Array<number> | number | string | Buffer | ArrayBuffer,
    encoding?: buffer$Encoding
  ): void;
  [i: number]: number;
  length: number;

  compare(otherBuffer: Buffer): number;
  copy(targetBuffer: Buffer, targetStart?: number, sourceStart?: number, sourceEnd?: number): number;
  entries(): Iterator<[number, number]>;
  equals(otherBuffer: Buffer): boolean;
  fill(value: string | Buffer | number, offset?: number, end?: number, encoding?: string): this;
  fill(value: string, encoding?: string): this;
  includes(
    value: string | Buffer | number,
    offsetOrEncoding?: number | buffer$Encoding,
    encoding?: buffer$Encoding
  ): boolean;
  indexOf(
    value: string | Buffer | number,
    offsetOrEncoding?: number | buffer$Encoding,
    encoding?: buffer$Encoding
  ): number;
  inspect(): string;
  keys(): Iterator<number>,
  lastIndexOf(
    value: string | Buffer | number,
    offsetOrEncoding?: number | buffer$Encoding,
    encoding?: buffer$Encoding
  ): number;
  readDoubleBE(offset?: number, noAssert?: boolean): number;
  readDoubleLE(offset?: number, noAssert?: boolean): number;
  readFloatBE(offset?: number, noAssert?: boolean): number;
  readFloatLE(offset?: number, noAssert?: boolean): number;
  readInt16BE(offset?: number, noAssert?: boolean): number;
  readInt16LE(offset?: number, noAssert?: boolean): number;
  readInt32BE(offset?: number, noAssert?: boolean): number;
  readInt32LE(offset?: number, noAssert?: boolean): number;
  readInt8(offset?: number, noAssert?: boolean): number;
  readIntBE(offset: number, byteLength: number, noAssert?: boolean): number;
  readIntLE(offset: number, byteLength: number, noAssert?: boolean): number;
  readUInt16BE(offset?: number, noAssert?: boolean): number;
  readUInt16LE(offset?: number, noAssert?: boolean): number;
  readUInt32BE(offset?: number, noAssert?: boolean): number;
  readUInt32LE(offset?: number, noAssert?: boolean): number;
  readUInt8(offset?: number, noAssert?: boolean): number;
  readUIntBE(offset: number, byteLength: number, noAssert?: boolean): number;
  readUIntLE(offset: number, byteLength: number, noAssert?: boolean): number;
  slice(start?: number, end?: number): this;
  swap16(): Buffer;
  swap32(): Buffer;
  swap64(): Buffer;
  toJSON(): buffer$ToJSONRet;
  toString(encoding?: buffer$Encoding, start?: number, end?: number): string;
  values(): Iterator<number>;
  write(string: string, offset?: number, length?: number, encoding?: buffer$Encoding): number;
  writeDoubleBE(value: number, offset?: number, noAssert?: boolean): number;
  writeDoubleLE(value: number, offset?: number, noAssert?: boolean): number;
  writeFloatBE(value: number, offset?: number, noAssert?: boolean): number;
  writeFloatLE(value: number, offset?: number, noAssert?: boolean): number;
  writeInt16BE(value: number, offset?: number, noAssert?: boolean): number;
  writeInt16LE(value: number, offset?: number, noAssert?: boolean): number;
  writeInt32BE(value: number, offset?: number, noAssert?: boolean): number;
  writeInt32LE(value: number, offset?: number, noAssert?: boolean): number;
  writeInt8(value: number, offset?: number, noAssert?: boolean): number;
  writeIntBE(value: number, offset: number, byteLength: number, noAssert?: boolean): number;
  writeIntLE(value: number, offset: number, byteLength: number, noAssert?: boolean): number;
  writeUInt16BE(value: number, offset?: number, noAssert?: boolean): number;
  writeUInt16LE(value: number, offset?: number, noAssert?: boolean): number;
  writeUInt32BE(value: number, offset?: number, noAssert?: boolean): number;
  writeUInt32LE(value: number, offset?: number, noAssert?: boolean): number;
  writeUInt8(value: number, offset?: number, noAssert?: boolean): number;
  writeUIntBE(value: number, offset: number, byteLength: number, noAssert?: boolean): number;
  writeUIntLE(value: number, offset: number, byteLength: number, noAssert?: boolean): number;

  static alloc(size: number, fill?: string | number, encoding?: buffer$Encoding): Buffer;
  static allocUnsafe(size: number): Buffer;
  static allocUnsafeSlow(size: number): Buffer;
  static byteLength(string: string | Buffer | $TypedArray | DataView | ArrayBuffer, encoding?: buffer$Encoding): number;
  static compare(buf1: Buffer, buf2: Buffer): number;
  static concat(list: Array<Buffer>, totalLength?: number): Buffer;

  static from(value: Buffer): Buffer;
  static from(value: string, encoding?: buffer$Encoding): Buffer;
  static from(value: ArrayBuffer | SharedArrayBuffer, byteOffset?: number, length?: number): Buffer;
  static from(value: Iterable<number>): this;
  static isBuffer(obj: any): boolean;
  static isEncoding(encoding: string): boolean;
}

declare type Node$Buffer = typeof Buffer;

declare module "buffer" {
  declare var kMaxLength: number;
  declare var INSPECT_MAX_BYTES: number;
  declare function transcode(source: Node$Buffer, fromEnc: buffer$Encoding, toEnc: buffer$Encoding): Node$Buffer;
  declare var Buffer: Node$Buffer;
}

type child_process$execOpts = {
  cwd?: string,
  env?: Object,
  encoding?: string,
  shell?: string,
  timeout?: number,
  maxBuffer?: number,
  killSignal?: string | number,
  uid?: number,
  gid?: number,
  windowsHide?: boolean,
  ...
};

declare class child_process$Error extends Error {
  code: number | string | null,
  errno?: string,
  syscall?: string,
  path?: string,
  spawnargs?: Array<string>,
  killed?: boolean,
  signal?: string | null,
  cmd: string,
}

type child_process$execCallback = (error: ?child_process$Error, stdout: string | Buffer, stderr: string | Buffer) => void;

type child_process$execSyncOpts = {
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

type child_process$execFileOpts = {
  cwd?: string,
  env?: Object,
  encoding?: string,
  timeout?: number,
  maxBuffer?: number,
  killSignal?: string | number,
  uid?: number,
  gid?: number,
  windowsHide?: boolean,
  windowsVerbatimArguments?: boolean,
  shell?: boolean | string,
  ...
};

type child_process$execFileCallback = (error: ?child_process$Error, stdout: string | Buffer, stderr: string | Buffer) => void;

type child_process$execFileSyncOpts = {
  cwd?: string,
  input?: string | Buffer | $TypedArray | DataView,
  stdio?: string | Array<any>,
  env?: Object,
  uid?: number,
  gid?: number,
  timeout?: number,
  killSignal?: string | number,
  maxBuffer?: number,
  encoding?: string,
  windowsHide?: boolean,
  shell?: boolean | string,
  ...
};

type child_process$forkOpts = {
  cwd?: string,
  env?: Object,
  execPath?: string,
  execArgv?: Array<string>,
  silent?: boolean,
  stdio?: Array<any> | string,
  windowsVerbatimArguments?: boolean,
  uid?: number,
  gid?: number,
  ...
};

type child_process$Handle = any; // TODO

type child_process$spawnOpts = {
  cwd?: string,
  env?: Object,
  argv0?: string,
  stdio?: string | Array<any>,
  detached?: boolean,
  uid?: number,
  gid?: number,
  shell?: boolean | string,
  windowsVerbatimArguments?: boolean,
  windowsHide?: boolean,
  ...
};

type child_process$spawnRet = {
  pid: number,
  output: Array<any>,
  stdout: Buffer | string,
  stderr: Buffer | string,
  status: number,
  signal: string,
  error: Error,
  ...
};

type child_process$spawnSyncOpts = {
  cwd?: string,
  input?: string | Buffer,
  stdio?: string | Array<any>,
  env?: Object,
  uid?: number,
  gid?: number,
  timeout?: number,
  killSignal?: string,
  maxBuffer?: number,
  encoding?: string,
  shell?: boolean | string,
  ...
};

type child_process$spawnSyncRet = child_process$spawnRet;

declare class child_process$ChildProcess extends events$EventEmitter {
  channel: Object;
  connected: boolean;
  killed: boolean;
  pid: number;
  exitCode: number | null;
  stderr: stream$Readable;
  stdin: stream$Writable;
  stdio: Array<any>;
  stdout: stream$Readable;

  disconnect(): void;
  kill(signal?: string): void;
  send(
    message: Object,
    sendHandleOrCallback?: child_process$Handle,
    optionsOrCallback?: Object | Function,
    callback?: Function
  ): boolean;
  unref(): void;
  ref(): void;
}

declare module "child_process" {
  declare var ChildProcess: typeof child_process$ChildProcess;

  declare function exec(
    command: string,
    optionsOrCallback?: child_process$execOpts | child_process$execCallback,
    callback?: child_process$execCallback
  ): child_process$ChildProcess;

  declare function execSync(
    command: string,
    options: { encoding: buffer$NonBufferEncoding, ... } & child_process$execSyncOpts
  ): string;

  declare function execSync(
    command: string,
    options?: child_process$execSyncOpts
  ): Buffer;

  declare function execFile(
    file: string,
    argsOrOptionsOrCallback?:
      Array<string> | child_process$execFileOpts | child_process$execFileCallback,
    optionsOrCallback?: child_process$execFileOpts | child_process$execFileCallback,
    callback?: child_process$execFileCallback
  ): child_process$ChildProcess;

  declare function execFileSync(
    command: string,
    argsOrOptions?: Array<string> | child_process$execFileSyncOpts,
    options?: child_process$execFileSyncOpts
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
    argsOrOptions?: Array<string> | child_process$spawnSyncOpts,
    options?: child_process$spawnSyncOpts
  ): child_process$spawnSyncRet;
}

declare module "cluster" {
  declare type ClusterSettings = {
    execArgv: Array<string>,
    exec: string,
    args: Array<string>,
    cwd: string,
    serialization: 'json' | 'advanced',
    silent: boolean,
    stdio: Array<any>,
    uid: number,
    gid: number,
    inspectPort: number | () => number,
    windowsHide: boolean,
    ...
  }

  declare type ClusterSettingsOpt = {
    execArgv?: Array<string>,
    exec?: string,
    args?: Array<string>,
    cwd?: string,
    serialization?: 'json' | 'advanced',
    silent?: boolean,
    stdio?: Array<any>,
    uid?: number,
    gid?: number,
    inspectPort?: number | () => number,
    windowsHide?: boolean,
    ...
  }

  declare class Worker extends events$EventEmitter {
    id: number;
    process: child_process$ChildProcess;
    suicide: boolean;

    disconnect(): void;
    isConnected(): boolean;
    isDead(): boolean;
    kill(signal?: string): void;
    send(
      message: Object,
      sendHandleOrCallback?: child_process$Handle | Function,
      callback?: Function,
    ): boolean;
  }

  declare class Cluster extends events$EventEmitter {
    isMaster: boolean;
    isWorker: boolean;
    settings: ClusterSettings;
    worker: Worker;
    workers: { [id: number]: Worker };

    disconnect(callback?: () => void): void;
    fork(env?: Object): Worker;
    setupMaster(settings?: ClusterSettingsOpt): void;
  }

  declare module.exports: Cluster;
}

type crypto$createCredentialsDetails = any; // TODO

declare class crypto$Cipher extends stream$Duplex {
  final(output_encoding: 'latin1' | 'binary' | 'base64' | 'hex'): string;
  final(output_encoding: void): Buffer;
  getAuthTag(): Buffer;
  setAAD(buffer: Buffer): crypto$Cipher;
  setAuthTag(buffer: Buffer): void;
  setAutoPadding(auto_padding?: boolean): crypto$Cipher;
  update(
    data: string,
    input_encoding: 'utf8' | 'ascii' | 'latin1' | 'binary',
    output_encoding: 'latin1' | 'binary' | 'base64' | 'hex'
  ): string;
  update(
    data: string,
    input_encoding: 'utf8' | 'ascii' | 'latin1' | 'binary',
    output_encoding: void
  ): Buffer;
  update(
    data: Buffer,
    input_encoding: void | 'utf8' | 'ascii' | 'latin1' | 'binary',
    output_encoding: 'latin1' | 'binary' | 'base64' | 'hex'
  ): string;
  update(
    data: Buffer,
    input_encoding: void,
    output_encoding: void
  ): Buffer;
}

type crypto$Credentials = {...}

type crypto$DiffieHellman = {
  computeSecret(
    other_public_key: string,
    input_encoding?: string,
    output_encoding?: string
  ): any,
  generateKeys(encoding?: string): any,
  getGenerator(encoding?: string): any,
  getPrime(encoding?: string): any,
  getPrivateKey(encoding?: string): any,
  getPublicKey(encoding?: string): any,
  setPrivateKey(private_key: any, encoding?: string): void,
  setPublicKey(public_key: any, encoding?: string): void,
  ...
}

type crypto$ECDH$Encoding = 'latin1' | 'hex' | 'base64';
type crypto$ECDH$Format = 'compressed' | 'uncompressed';

declare class crypto$ECDH {
  computeSecret(
    other_public_key: Buffer | $TypedArray | DataView
  ): Buffer,
  computeSecret(
    other_public_key: string,
    input_encoding: crypto$ECDH$Encoding
  ): Buffer,
  computeSecret(
    other_public_key: Buffer | $TypedArray | DataView,
    output_encoding: crypto$ECDH$Encoding
  ): string,
  computeSecret(
    other_public_key: string,
    input_encoding: crypto$ECDH$Encoding,
    output_encoding: crypto$ECDH$Encoding
  ): string,
  generateKeys(format?: crypto$ECDH$Format): Buffer,
  generateKeys(encoding: crypto$ECDH$Encoding, format?: crypto$ECDH$Format): string,
  getPrivateKey(): Buffer,
  getPrivateKey(encoding: crypto$ECDH$Encoding): string,
  getPublicKey(format?: crypto$ECDH$Format): Buffer,
  getPublicKey(encoding: crypto$ECDH$Encoding, format?: crypto$ECDH$Format): string,
  setPrivateKey(private_key: Buffer | $TypedArray | DataView): void,
  setPrivateKey(private_key: string, encoding: crypto$ECDH$Encoding): void
}

declare class crypto$Decipher extends stream$Duplex {
  final(output_encoding: 'latin1' | 'binary' | 'ascii' | 'utf8'): string;
  final(output_encoding: void): Buffer;
  getAuthTag(): Buffer;
  setAAD(buffer: Buffer): void;
  setAuthTag(buffer: Buffer): void;
  setAutoPadding(auto_padding?: boolean): crypto$Cipher;
  update(
    data: string,
    input_encoding: 'latin1' | 'binary' | 'base64' | 'hex',
    output_encoding: 'latin1' | 'binary' | 'ascii' | 'utf8',
  ): string;
  update(
    data: string,
    input_encoding: 'latin1' | 'binary' | 'base64' | 'hex',
    output_encoding: void
  ): Buffer;
  update(
    data: Buffer,
    input_encoding: void,
    output_encoding: 'latin1' | 'binary' | 'ascii' | 'utf8',
  ): string;
  update(
    data: Buffer,
    input_encoding: void,
    output_encoding: void
  ): Buffer;
}

declare class crypto$Hash extends stream$Duplex {
  digest(encoding: 'hex' | 'latin1' | 'binary' | 'base64'): string;
  digest(encoding: 'buffer'): Buffer;
  digest(encoding: void): Buffer;
  update(data: string | Buffer, input_encoding?: 'utf8' | 'ascii' | 'latin1' |
  'binary'): crypto$Hash;
}

declare class crypto$Hmac extends stream$Duplex {
  digest(encoding: 'hex' | 'latin1' | 'binary' | 'base64'): string;
  digest(encoding: 'buffer'): Buffer;
  digest(encoding: void): Buffer;
  update(data: string | Buffer, input_encoding?: 'utf8' | 'ascii' | 'latin1' |
  'binary'): crypto$Hmac;
}

type crypto$Sign$private_key = string | {
  key: string,
  passphrase: string,
  ...
}
declare class crypto$Sign extends stream$Writable {
  static(algorithm: string, options?: writableStreamOptions): crypto$Sign,
  constructor(algorithm: string, options?: writableStreamOptions): void;
  sign(
    private_key: crypto$Sign$private_key,
    output_format: 'latin1' | 'binary' | 'hex' | 'base64'
  ): string;
  sign(
    private_key: crypto$Sign$private_key,
    output_format: void
  ): Buffer;
  update(data: string | Buffer, input_encoding?: 'utf8' | 'ascii' | 'latin1' |
  'binary'): crypto$Sign;
}

declare class crypto$Verify extends stream$Writable {
  static(algorithm: string, options?: writableStreamOptions): crypto$Verify,
  constructor(algorithm: string, options?: writableStreamOptions): void;
  update(data: string | Buffer, input_encoding?: 'utf8' | 'ascii' | 'latin1' |
  'binary' ): crypto$Verify;
  verify(
    object: string,
    signature: string | Buffer | $TypedArray | DataView,
    signature_format: 'latin1' | 'binary' | 'hex' | 'base64'
  ): boolean;
  verify(object: string, signature: Buffer, signature_format: void): boolean;
}


type crypto$key = string | {
  key: string,
  passphrase?: string,
  // TODO: enum type in crypto.constants
  padding?: string,
  ...
};

declare module "crypto" {
  declare var DEFAULT_ENCODING: string;

  declare class Sign extends crypto$Sign {}
  declare class Verify extends crypto$Verify {}

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
  declare function createECDH(curveName: string): crypto$ECDH;
  declare function createHash(algorithm: string): crypto$Hash;
  declare function createHmac(algorithm: string, key: string | Buffer): crypto$Hmac;
  declare function createSign(algorithm: string): crypto$Sign;
  declare function createVerify(algorithm: string): crypto$Verify;
  declare function getCiphers(): Array<string>;
  declare function getCurves(): Array<string>;
  declare function getDiffieHellman(group_name: string): crypto$DiffieHellman;
  declare function getHashes(): Array<string>;
  declare function pbkdf2(
    password: string | Buffer,
    salt: string | Buffer,
    iterations: number,
    keylen: number,
    digest: string,
    callback: (err: ?Error, derivedKey: Buffer) => void
  ): void;
  declare function pbkdf2(
    password: string | Buffer,
    salt: string | Buffer,
    iterations: number,
    keylen: number,
    callback: (err: ?Error, derivedKey: Buffer) => void
  ): void;
  declare function pbkdf2Sync(
    password: string | Buffer,
    salt: string | Buffer,
    iterations: number,
    keylen: number,
    digest?: string
  ): Buffer;
  declare function scrypt(
    password: string | Buffer,
    salt: string | Buffer,
    keylen: number,
    options: {|N?: number, r?: number, p?: number, maxmem?: number|}
        | {|cost?: number, blockSize?: number, parallelization?: number, maxmem?: number|},
    callback: (err: ?Error, derivedKey: Buffer) => void
  ): void;
  declare function scrypt(
    password: string | Buffer,
    salt: string | Buffer,
    keylen: number,
    callback: (err: ?Error, derivedKey: Buffer) => void
  ): void;
  declare function scryptSync(
    password: string | Buffer,
    salt: string | Buffer,
    keylen: number,
    options?: {|N?: number, r?: number, p?: number, maxmem?: number|}
        | {|cost?: number, blockSize?: number, parallelization?: number, maxmem?: number|},
  ): Buffer;
  declare function privateDecrypt(
    private_key: crypto$key,
    buffer: Buffer
  ): Buffer;
  declare function privateEncrypt(
    private_key: crypto$key,
    buffer: Buffer
  ): Buffer;
  declare function publicDecrypt(
    key: crypto$key,
    buffer: Buffer
  ): Buffer;
  declare function publicEncrypt(
    key: crypto$key,
    buffer: Buffer
  ): Buffer;
  // `UNUSED` argument strictly enforces arity to enable overloading this
  // function with 1-arg and 2-arg variants.
  declare function pseudoRandomBytes(size: number, UNUSED: void): Buffer;
  declare function pseudoRandomBytes(
    size: number,
    callback: (err: ?Error, buffer: Buffer) => void
  ): void;
  // `UNUSED` argument strictly enforces arity to enable overloading this
  // function with 1-arg and 2-arg variants.
  declare function randomBytes(size: number, UNUSED: void): Buffer;
  declare function randomBytes(
    size: number,
    callback: (err: ?Error, buffer: Buffer) => void
  ): void;
  declare function randomFillSync(buffer: Buffer | $TypedArray | DataView): void
  declare function randomFillSync(buffer: Buffer | $TypedArray | DataView, offset: number): void
  declare function randomFillSync(
    buffer: Buffer | $TypedArray | DataView,
    offset: number,
    size: number
  ): void
  declare function randomFill(
    buffer: Buffer | $TypedArray | DataView,
    callback: (err: ?Error, buffer: Buffer) => void
  ): void
  declare function randomFill(
    buffer: Buffer | $TypedArray | DataView,
    offset: number,
    callback: (err: ?Error, buffer: Buffer) => void
  ): void
  declare function randomFill(
    buffer: Buffer | $TypedArray | DataView,
    offset: number,
    size: number,
    callback: (err: ?Error, buffer: Buffer) => void
  ): void
  declare function timingSafeEqual(
    a: Buffer | $TypedArray | DataView,
    b: Buffer | $TypedArray | DataView
  ): boolean;
}

type net$Socket$address = {
  address: string,
  family: string,
  port: number,
  ...
};
type dgram$Socket$rinfo = {
  address: string,
  family: 'IPv4' | 'IPv6',
  port: number,
  size: number,
  ...
};

declare class dgram$Socket extends events$EventEmitter {
  addMembership(multicastAddress: string, multicastInterface?: string): void;
  address(): net$Socket$address;
  bind(port?: number, address?: string, callback?: () => void): void;
  close(callback?: () => void): void;
  dropMembership(multicastAddress: string, multicastInterface?: string): void;
  ref(): void;
  send(
    msg: Buffer,
    port: number,
    address: string,
    callback?: (err: ?Error, bytes: any) => mixed,
  ): void;
  send(
    msg: Buffer,
    offset: number,
    length: number,
    port: number,
    address: string,
    callback?: (err: ?Error, bytes: any) => mixed,
  ): void;
  setBroadcast(flag: boolean): void;
  setMulticastLoopback(flag: boolean): void;
  setMulticastTTL(ttl: number): void;
  setTTL(ttl: number): void;
  unref(): void;
};

declare module "dgram" {
  declare function createSocket(
    options: string | { type: string, ... },
    callback?: () => void
  ): dgram$Socket;
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
  declare var ADDRCONFIG: number;
  declare var V4MAPPED: number;

  declare type LookupOptions = {
    family?: number,
    hints?: number,
    verbatim?: boolean,
    all?: boolean,
    ...
  };

  declare function lookup(
    domain: string,
    options: number | LookupOptions,
    callback: (err: ?Error, address: string, family: number) => void
  ): void;
  declare function lookup(
    domain: string,
    callback: (err: ?Error, address: string, family: number) => void
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
  declare function timingSafeEqual(
    a: Buffer | $TypedArray | DataView,
    b: Buffer | $TypedArray | DataView
  ): boolean;
}

declare class events$EventEmitter {
  // deprecated
  static listenerCount(emitter: events$EventEmitter, event: string): number;
  static defaultMaxListeners: number;

  addListener(event: string, listener: Function): this;
  emit(event: string, ...args:Array<any>): boolean;
  eventNames(): Array<string>;
  listeners(event: string): Array<Function>;
  listenerCount(event: string): number;
  on(event: string, listener: Function): this;
  once(event: string, listener: Function): this;
  prependListener(event: string, listener: Function): this;
  prependOnceListener(event: string, listener: Function): this;
  removeAllListeners(event?: string): this;
  removeListener(event: string, listener: Function): this;
  off(event: string, listener: Function): this;
  setMaxListeners(n: number): this;
  getMaxListeners(): number;
  rawListeners(event: string): Array<Function>;
}


declare module "events" {
  // TODO: See the comment above the events$EventEmitter declaration
  declare class EventEmitter extends events$EventEmitter {
    static EventEmitter: typeof EventEmitter;
  }

  declare module.exports: typeof EventEmitter;
}

declare class domain$Domain extends events$EventEmitter {
  members: Array<any>;

  add(emitter: events$EventEmitter): void;
  bind(callback: Function): Function;
  dispose(): void;
  enter(): void;
  exit(): void;
  intercept(callback: Function): Function;
  remove(emitter: events$EventEmitter): void;
  run(fn: Function): void;
}

declare module "domain" {
  declare function create(): domain$Domain;
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
    atimeMs: number;
    mtimeMs: number;
    ctimeMs: number;
    birthtimeMs: number;
    atime: Date;
    mtime: Date;
    ctime: Date;
    birthtime: Date;

    isFile(): boolean;
    isDirectory(): boolean;
    isBlockDevice(): boolean;
    isCharacterDevice(): boolean;
    isSymbolicLink(): boolean;
    isFIFO(): boolean;
    isSocket(): boolean;
  }

  declare class FSWatcher extends events$EventEmitter {
    close(): void;
  }

  declare class ReadStream extends stream$Readable {
    close(): void;
  }

  declare class WriteStream extends stream$Writable {
    close(): void;
    bytesWritten: number;
  }

  declare class Dirent {
    name: string | Buffer;

    isBlockDevice(): boolean;
    isCharacterDevice(): boolean;
    isDirectory(): boolean;
    isFIFO(): boolean;
    isFile(): boolean;
    isSocket(): boolean;
    isSymbolicLink(): boolean;
  }

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
  declare function symlinkSync(srcpath: string, dstpath: string, type?: string): void;
  declare function readlink(path: string, callback: (err: ?ErrnoError, linkString: string) => void): void;
  declare function readlinkSync(path: string): string;
  declare function realpath(path: string, cache?: Object, callback?: (err: ?ErrnoError, resolvedPath: string) => void): void;
  declare function realpathSync(path: string, cache?: Object): string;
  declare function unlink(path: string, callback?: (err: ?ErrnoError) => void): void;
  declare function unlinkSync(path: string): void;
  declare function rmdir(path: string, callback?: (err: ?ErrnoError) => void): void;
  declare function rmdirSync(path: string): void;
  declare function mkdir(path: string, mode?: number | {
    recursive?: boolean,
    mode?: number,
    ...
  }, callback?: (err: ?ErrnoError) => void): void;
  declare function mkdirSync(path: string, mode?: number | {
    recursive?: boolean,
    mode?: number,
    ...
  }): void;
  declare function mkdtemp(prefix: string, callback: (err: ?ErrnoError, folderPath: string) => void): void;
  declare function mkdtempSync(prefix: string): string;
  declare function readdir(
    path: string,
    options: string | { encoding?: string, withFileTypes?: false, ... },
    callback: (err: ?ErrnoError, files: Array<string>) => void
  ): void;
  declare function readdir(
    path: string,
    options: { encoding?: string, withFileTypes?: true, ... },
    callback: (err: ?ErrnoError, files: Array<Dirent>) => void
  ): void;
  declare function readdir(
    path: string,
    callback: (err: ?ErrnoError, files: Array<string>) => void
  ): void;
  declare function readdirSync(
    path: string,
    options?: string | { encoding?: string, withFileTypes?: false, ... }
  ): Array<string>;
  declare function readdirSync(
    path: string,
    options?: string | { encoding?: string, withFileTypes?: true, ... }
  ): Array<Dirent>;
  declare function close(fd: number, callback: (err: ?ErrnoError) => void): void;
  declare function closeSync(fd: number): void;
  declare function open(
    path: string | Buffer | URL,
    flags: string | number,
    mode: number,
    callback: (err: ?ErrnoError, fd: number) => void
  ): void;
  declare function open(
    path: string | Buffer | URL,
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
  declare function write(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: number,
    callback: (err: ?ErrnoError, write: number, buf: Buffer) => void
  ): void;
  declare function write(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    callback: (err: ?ErrnoError, write: number, buf: Buffer) => void
  ): void;
  declare function write(
    fd: number,
    buffer: Buffer,
    offset: number,
    callback: (err: ?ErrnoError, write: number, buf: Buffer) => void
  ): void;
  declare function write(
    fd: number,
    buffer: Buffer,
    callback: (err: ?ErrnoError, write: number, buf: Buffer) => void
  ): void;
  declare function write(
    fd: number,
    data: string,
    position: number,
    encoding: string,
    callback: (err: ?ErrnoError, write: number, str: string) => void
  ): void;
  declare function write(
    fd: number,
    data: string,
    position: number,
    callback: (err: ?ErrnoError, write: number, str: string) => void
  ): void;
  declare function write(
    fd: number,
    data: string,
    callback: (err: ?ErrnoError, write: number, str: string) => void
  ): void;
  declare function writeSync(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: number,
  ): number;
  declare function writeSync(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
  ): number;
  declare function writeSync(
    fd: number,
    buffer: Buffer,
    offset?: number,
  ): number;
  declare function writeSync(
    fd: number,
    str: string,
    position: number,
    encoding: string,
  ): number;
  declare function writeSync(
    fd: number,
    str: string,
    position?: number,
  ): number;
  declare function read(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: ?number,
    callback: (err: ?ErrnoError, bytesRead: number, buffer: Buffer) => void
  ): void;
  declare function readSync(
    fd: number,
    buffer: Buffer,
    offset: number,
    length: number,
    position: number
  ): number;
  declare function readFile(
    path: string | Buffer | URL | number,
    callback: (err: ?ErrnoError, data: Buffer) => void
  ): void;
  declare function readFile(
    path: string | Buffer | URL | number,
    encoding: string,
    callback: (err: ?ErrnoError, data: string) => void
  ): void;
  declare function readFile(
    path: string | Buffer | URL | number,
    options: {
      encoding: string,
      flag?: string,
      ...
    },
    callback: (err: ?ErrnoError, data: string) => void
  ): void;
  declare function readFile(
    path: string | Buffer | URL | number,
    options: { flag?: string, ... },
    callback: (err: ?ErrnoError, data: Buffer) => void
  ): void;
  declare function readFileSync(
    path: string | Buffer | URL | number
  ): Buffer;
  declare function readFileSync(
    path: string | Buffer | URL | number,
    encoding: string
  ): string;
  declare function readFileSync(path: string | Buffer | URL | number, options: {
    encoding: string,
    flag?: string,
    ...
  }): string;
  declare function readFileSync(path: string | Buffer | URL | number, options: {
    encoding?: void,
    flag?: string,
    ...
  }): Buffer;
  declare function writeFile(
    filename: string | Buffer | number,
    data: Buffer | string,
    options: string | {
      encoding?: ?string,
      mode?: number,
      flag?: string,
      ...
    },
    callback: (err: ?ErrnoError) => void
  ): void;
  declare function writeFile(
    filename: string | Buffer | number,
    data: Buffer | string,
    callback?: (err: ?ErrnoError) => void
  ): void;
  declare function writeFileSync(
    filename: string,
    data: Buffer | string,
    options?: string | {
      encoding?: ?string,
      mode?: number,
      flag?: string,
      ...
    }
  ): void;
  declare function appendFile(
    filename: string | Buffer | number,
    data: string | Buffer,
    options: string | {
      encoding?: ?string,
      mode?: number,
      flag?: string,
      ...
    },
    callback: (err: ?ErrnoError) => void
  ): void;
  declare function appendFile(
    filename: string | Buffer | number,
    data: string | Buffer,
    callback: (err: ?ErrnoError) => void
  ): void;
  declare function appendFileSync(
    filename: string | Buffer | number,
    data: string | Buffer,
    options?: string | {
      encoding?: ?string,
      mode?: number,
      flag?: string,
      ...
    }
  ): void;
  declare function watchFile(filename: string, options?: Object, listener?: (curr: Stats, prev: Stats) => void): void;
  declare function unwatchFile(filename: string, listener?: (curr: Stats, prev: Stats) => void): void;
  declare function watch(filename: string, options?: Object, listener?: (event: string, filename: string) => void): FSWatcher;
  declare function exists(path: string, callback?: (exists: boolean) => void): void;
  declare function existsSync(path: string): boolean;
  declare function access(path: string, mode?: number, callback?: (err: ?ErrnoError) => void): void;
  declare function accessSync(path: string, mode?: number): void;
  declare function createReadStream(path: string, options?: Object): ReadStream;
  declare function createWriteStream(path: string, options?: Object): WriteStream;
  declare function fdatasync(fd: number, callback: (err: ?ErrnoError) => void): void;
  declare function fdatasyncSync(fd: number): void;
  declare function copyFile(src: string, dest: string, flags?: number, callback: (err: ErrnoError) => void): void;
  declare function copyFileSync(src: string, dest: string, flags?: number): void;

  declare var F_OK: number;
  declare var R_OK: number;
  declare var W_OK: number;
  declare var X_OK: number;
  // new var from node 6.x
  // https://nodejs.org/dist/latest-v6.x/docs/api/fs.html#fs_fs_constants_1
  declare var constants: {
    F_OK: number, // 0
    R_OK: number, // 4
    W_OK: number, // 2
    X_OK: number, // 1
    COPYFILE_EXCL: number, // 1
    COPYFILE_FICLONE: number, // 2
    COPYFILE_FICLONE_FORCE: number, // 4
    O_RDONLY: number, // 0
    O_WRONLY: number, // 1
    O_RDWR: number, // 2
    S_IFMT: number, // 61440
    S_IFREG: number, // 32768
    S_IFDIR: number, // 16384
    S_IFCHR: number, // 8192
    S_IFBLK: number, // 24576
    S_IFIFO: number, // 4096
    S_IFLNK: number, // 40960
    S_IFSOCK: number, // 49152
    O_CREAT: number, // 64
    O_EXCL: number, // 128
    O_NOCTTY: number, // 256
    O_TRUNC: number, // 512
    O_APPEND: number, // 1024
    O_DIRECTORY: number, // 65536
    O_NOATIME: number, // 262144
    O_NOFOLLOW: number, // 131072
    O_SYNC: number, // 1052672
    O_DSYNC: number, // 4096
    O_SYMLINK: number, // 2097152
    O_DIRECT: number, // 16384
    O_NONBLOCK: number, // 2048
    S_IRWXU: number, // 448
    S_IRUSR: number, // 256
    S_IWUSR: number, // 128
    S_IXUSR: number, // 64
    S_IRWXG: number, // 56
    S_IRGRP: number, // 32
    S_IWGRP: number, // 16
    S_IXGRP: number, // 8
    S_IRWXO: number, // 7
    S_IROTH: number, // 4
    S_IWOTH: number, // 2
    S_IXOTH: number, // 1
    ...
  };

  declare type BufferEncoding =
    | 'buffer'
    | { encoding: 'buffer', ... };
  declare type EncodingOptions = { encoding?: string, ... };
  declare type EncodingFlag = EncodingOptions & { flag?: string, ... };
  declare type WriteOptions = EncodingFlag & { mode?: number, ... };
  declare class FileHandle {
    appendFile(data: string | Buffer, options: WriteOptions | string): Promise<void>;
    chmod(mode: number): Promise<void>;
    chown(uid: number, guid: number): Promise<void>;
    close(): Promise<void>;
    datasync(): Promise<void>;
    fd: number;
    read<T: Buffer | Uint8Array>(
      buffer: T,
      offset: number,
      length: number,
      position: number
    ): Promise<{
      bytesRead: number,
      buffer: T,
      ...
    }>;
    readFile(options: EncodingFlag): Promise<Buffer>;
    readFile(options: string): Promise<string>;
    stat(): Promise<Stats>;
    sync(): Promise<void>;
    truncate(len?: number): Promise<void>;
    utimes(atime: number | string | Date, mtime: number | string | Date): Promise<void>;
    write(buffer: Buffer | Uint8Array, offset: number, length: number, position: number): Promise<void>;
    writeFile(data: string | Buffer | Uint8Array, options: WriteOptions | string): Promise<void>;
  }

  declare type FSPromisePath = string | Buffer | URL;
  declare type FSPromise = {
    access(path: FSPromisePath, mode?: number): Promise<void>,
    appendFile(path: FSPromisePath | FileHandle, data: string | Buffer, options: WriteOptions | string): Promise<void>,
    chmod(path: FSPromisePath, mode: number): Promise<void>,
    chown(path: FSPromisePath, uid: number, gid: number): Promise<void>,
    copyFile(src: FSPromisePath, dest: FSPromisePath, flags?: number): Promise<void>,
    fchmod(filehandle: FileHandle, mode: number): Promise<void>,
    fchown(filehandle: FileHandle, uid: number, guid: number): Promise<void>,
    fdatasync(filehandle: FileHandle): Promise<void>,
    fstat(filehandle: FileHandle): Promise<Stats>,
    fsync(filehandle: FileHandle): Promise<void>,
    ftruncate(filehandle: FileHandle, len?: number): Promise<void>,
    futimes(filehandle: FileHandle, atime: number | string | Date, mtime: number | string | Date): Promise<void>,
    lchmod(path: FSPromisePath, mode: number): Promise<void>,
    lchown(path: FSPromisePath, uid: number, guid: number): Promise<void>,
    link(existingPath: FSPromisePath, newPath: FSPromisePath): Promise<void>,
    mkdir(path: FSPromisePath, mode?: number | {
      recursive?: boolean,
      mode?: number,
      ...
    }): Promise<void>,
    mkdtemp(prefix: string, options: EncodingOptions): Promise<string>,
    open(path: FSPromisePath, flags?: string | number, mode?: number): Promise<FileHandle>,
    read<T: Buffer | Uint8Array>(
      filehandle: FileHandle,
      buffer: T,
      offset: number,
      length: number,
      position?: number
    ): Promise<{
      bytesRead: number,
      buffer: T,
      ...
    }>,
    readdir(path: FSPromisePath, options?: string | EncodingOptions): Promise<string[]>,
    readFile:
      & ((path: FSPromisePath | FileHandle, options: string) => Promise<string>)
      & ((path: FSPromisePath | FileHandle, options?: EncodingFlag) => Promise<Buffer>),
    readlink:
      & ((path: FSPromisePath, options: BufferEncoding) => Promise<Buffer>)
      & ((path: FSPromisePath, options?: string | EncodingOptions) => Promise<string>),
    realpath:
      & ((path: FSPromisePath, options: BufferEncoding) => Promise<Buffer>)
      & ((path: FSPromisePath, options?: string | EncodingOptions) => Promise<string>),
    rename(oldPath: FSPromisePath, newPath: FSPromisePath): Promise<void>,
    rmdir(path: FSPromisePath): Promise<void>,
    stat(path: FSPromisePath): Promise<Stats>,
    symlink(target: FSPromisePath, path: FSPromisePath, type?: 'dir' | 'file' | 'junction'): Promise<void>,
    truncate(path: FSPromisePath, len?: number): Promise<void>,
    unlink(path: FSPromisePath): Promise<void>,
    utimes(path: FSPromisePath, atime: number | string | Date, mtime: number | string | Date): Promise<void>,
    write<T: Buffer | Uint8Array>(
      filehandle: FileHandle,
      buffer: T,
      offset: number,
      length: number,
      position?: number
    ): Promise<{
      bytesRead: number,
      buffer: T,
      ...
    }>,
    writeFile(
      FSPromisePath | FileHandle,
      data: string | Buffer | Uint8Array,
      options?: string | WriteOptions
    ): Promise<void>,
    ...
  }

  declare var promises: FSPromise;
}

type http$agentOptions = {
  keepAlive?: boolean,
  keepAliveMsecs?: number,
  maxSockets?: number,
  maxFreeSockets?: number,
  ...
}

declare class http$Agent<+SocketT = net$Socket> {
  constructor(options: http$agentOptions): void;
  destroy(): void;
  freeSockets: { [name: string]: $ReadOnlyArray<SocketT>, ... };
  getName(options: {
    host: string,
    port: number,
    localAddress: string,
    ...
  }): string;
  maxFreeSockets: number;
  maxSockets: number;
  requests: { [name: string]: $ReadOnlyArray<http$ClientRequest<SocketT>>, ... };
  sockets: { [name: string]: $ReadOnlyArray<SocketT>, ... };
}

declare class http$IncomingMessage<SocketT = net$Socket> extends stream$Readable {
  headers: Object;
  rawHeaders: Array<string>;
  httpVersion: string;
  method: string;
  trailers: Object;
  setTimeout(msecs: number, callback: Function): void;
  socket: SocketT;
  statusCode: number;
  statusMessage: string;
  url: string;
  aborted: boolean;
  complete: boolean;
  rawTrailers: Array<string>;
}

declare class http$ClientRequest<+SocketT = net$Socket> extends stream$Writable {
  abort(): void;
  aborted: boolean;
  +connection: SocketT | null;
  flushHeaders(): void;
  getHeader(name: string): string;
  removeHeader(name: string): void;
  setHeader(name: string, value: string | Array<string>): void;
  setNoDelay(noDelay?: boolean): void;
  setSocketKeepAlive(enable?: boolean, initialDelay?: number): void;
  setTimeout(msecs: number, callback?: Function): void;
  +socket: SocketT | null;
}

declare class http$ServerResponse extends stream$Writable {
  addTrailers(headers: { [key: string] : string, ... }): void;
  connection: net$Socket;
  finished: boolean;
  flushHeaders(): void;
  getHeader(name: string): string;
  getHeaderNames(): Array<string>;
  getHeaders(): { [key: string] : string | Array<string>, ...};
  hasHeader(name: string): boolean;
  headersSent: boolean;
  removeHeader(name: string): void;
  sendDate: boolean;
  setHeader(name: string, value: string | Array<string>): void;
  setTimeout(msecs: number, callback?: Function): http$ServerResponse;
  socket: net$Socket;
  statusCode: number;
  statusMessage: string;
  writeContinue(): void;
  writeHead(status: number, statusMessage?: string, headers?: { [key: string] : string, ... }): void;
  writeHead(status: number, headers?: { [key: string] : string, ... }): void;
  writeProcessing(): void;
}

declare class http$Server extends net$Server {
  listen(port?: number, hostname?: string, backlog?: number, callback?: Function): this;
  // The following signatures are added to allow omitting intermediate arguments
  listen(port?: number, backlog?: number, callback?: Function): this;
  listen(port?: number, hostname?: string, callback?: Function): this;
  listen(port?: number, callback?: Function): this;
  listen(path: string, callback?: Function): this;
  listen(handle: {
    port?: number,
    host?: string,
    path?: string,
    backlog?: number,
    exclusive?: boolean,
    readableAll?: boolean,
    writableAll?: boolean,
    ipv6Only?: boolean,
    ...
  }, callback?: Function): this;
  listening: boolean;
  close(callback?: (error: ?Error) => mixed): this;
  maxHeadersCount: number;
  keepAliveTimeout: number;
  headersTimeout: number;
  setTimeout(msecs: number, callback: Function): this;
  timeout: number;
}

declare class https$Server extends tls$Server {
  listen(port?: number, hostname?: string, backlog?: number, callback?: Function): this;
  // The following signatures are added to allow omitting intermediate arguments
  listen(port?: number, backlog?: number, callback?: Function): this;
  listen(port?: number, hostname?: string, callback?: Function): this;
  listen(port?: number, callback?: Function): this;
  listen(path: string, callback?: Function): this;
  listen(handle: {
    port?: number,
    host?: string,
    path?: string,
    backlog?: number,
    exclusive?: boolean,
    readableAll?: boolean,
    writableAll?: boolean,
    ipv6Only?: boolean,
    ...
  }, callback?: Function): this;
  close(callback?: (error: ?Error) => mixed): this;
  keepAliveTimeout: number;
  headersTimeout: number;
  setTimeout(msecs: number, callback: Function): this;
  timeout: number;
}

type requestOptions = {|
  auth?: string,
  defaultPort?: number,
  family?: number,
  headers?: { [key: string] : mixed, ... },
  host?: string,
  hostname?: string,
  localAddress?: string,
  method?: string,
  path?: string,
  port?: number,
  protocol?: string,
  setHost?: boolean,
  socketPath?: string,
  timeout?: number,
|}

type http$requestOptions = {
  ...requestOptions,
  agent?: boolean | http$Agent<net$Socket>,
  createConnection?: (options: net$connectOptions, callback?: Function) => net$Socket,
  ...
};

declare module "http" {
  declare class Server extends http$Server {}
  declare class Agent extends http$Agent<net$Socket> {
    createConnection(options: net$connectOptions, callback?: Function): net$Socket;
  }
  declare class ClientRequest extends http$ClientRequest<net$Socket> {}
  declare class IncomingMessage extends http$IncomingMessage<net$Socket> {}
  declare class ServerResponse extends http$ServerResponse {}

  declare function createServer(
    requestListener?: (request: IncomingMessage, response: ServerResponse) => void
  ): Server;
  declare function request(
    options: http$requestOptions,
    callback?: (response: IncomingMessage) => void
  ): ClientRequest;
  declare function request(
    url: string,
    options?: http$requestOptions,
    callback?: (response: IncomingMessage) => void
  ): ClientRequest;
  declare function get(
    options: http$requestOptions,
    callback?: (response: IncomingMessage) => void
  ): ClientRequest;
  declare function get(
    url: string,
    options?: http$requestOptions,
    callback?: (response: IncomingMessage) => void
  ): ClientRequest;

  declare var METHODS: Array<string>;
  declare var STATUS_CODES: { [key: number]: string, ... };
  declare var globalAgent: Agent;
}

type https$requestOptions = {
  ...requestOptions,
  agent?: boolean | http$Agent<tls$TLSSocket>,
  createConnection?: (options: tls$connectOptions, callback?: Function) => tls$TLSSocket,
  ...
};

declare module "https" {
  declare class Server extends https$Server {}
  declare class Agent extends http$Agent<tls$TLSSocket> {
    createConnection(port: ?number, host: ?string, options: tls$connectOptions): tls$TLSSocket;
    createConnection(port: ?number, options: tls$connectOptions): tls$TLSSocket;
    createConnection(options: tls$connectOptions): tls$TLSSocket;
  }

  declare class ClientRequest extends http$ClientRequest<tls$TLSSocket> {}
  declare class IncomingMessage extends http$IncomingMessage<tls$TLSSocket> {}
  declare class ServerResponse extends http$ServerResponse {}

  declare function createServer(
    options: Object,
    requestListener?: (request: IncomingMessage, response: ServerResponse) => void
  ): Server;
  declare function request(
    options: https$requestOptions,
    callback?: (response: IncomingMessage) => void
  ): ClientRequest;
  declare function request(
    url: string,
    options?: https$requestOptions,
    callback?: (response: IncomingMessage) => void
  ): ClientRequest;
  declare function get(
    options: https$requestOptions,
    callback?: (response: IncomingMessage) => void
  ): ClientRequest;
  declare function get(
    url: string,
    options?: https$requestOptions,
    callback?: (response: IncomingMessage) => void
  ): ClientRequest;

  declare var globalAgent: Agent;
}

type module$Module = {
  builtinModules: Array<string>,
  createRequire(filename: string | URL): typeof require,
  syncBuiltinESMExports(): void,
  Module: module$Module,
  ...
};

declare module "module" {
  declare module.exports: module$Module;
}

declare class net$Socket extends stream$Duplex {
  constructor(options?: Object): void;
  address(): net$Socket$address;
  bufferSize: number;
  bytesRead: number;
  bytesWritten: number;
  connect(path: string, connectListener?: () => mixed): net$Socket;
  connect(port: number, host?: string, connectListener?: () => mixed): net$Socket;
  connect(port: number, connectListener?: () => mixed): net$Socket;
  connect(options: Object, connectListener?: () => mixed): net$Socket;
  destroyed: boolean;
  end(
    chunkOrEncodingOrCallback?: Buffer | Uint8Array | string | (data: any) => void,
    encodingOrCallback?: string | (data: any) => void,
    callback?: (data: any) => void
  ): this;
  localAddress: string;
  localPort: number;
  pause(): this;
  ref(): this;
  remoteAddress: string | void;
  remoteFamily: string;
  remotePort: number;
  resume(): this;
  setEncoding(encoding?: string): this;
  setKeepAlive(enable?: boolean, initialDelay?: number): this;
  setNoDelay(noDelay?: boolean): this;
  setTimeout(timeout: number, callback?: Function): this;
  unref(): this;
  write(
    chunk: Buffer | Uint8Array | string,
    encodingOrCallback?: string | (data: any) => void,
    callback?: (data: any) => void
  ): boolean;
}

declare class net$Server extends events$EventEmitter {
  listen(port?: number, hostname?: string, backlog?: number, callback?: Function): net$Server;
  listen(path: string, callback?: Function): net$Server;
  listen(handle: Object, callback?: Function): net$Server;
  listening: boolean;
  close(callback?: Function): net$Server;
  address(): net$Socket$address;
  connections: number;
  maxConnections: number;
  getConnections(callback: Function): void;
  ref():  net$Server;
  unref():  net$Server;
}


type net$connectOptions = {
  port?: number,
  host?: string,
  localAddress?: string,
  localPort?: number,
  family?: number,
  lookup?: (
    domain: string,
    options?: ?number | ?Object,
    callback?: (err: ?Error, address: string, family: number) => void
  ) => mixed,
  path?: string,
  ...
};

declare module "net" {

  declare class Server extends net$Server {}
  declare class Socket extends net$Socket {}

  declare function isIP(input: string): number;
  declare function isIPv4(input: string): boolean;
  declare function isIPv6(input: string): boolean;


  declare type connectionListener = (socket: Socket) => any;
  declare function createServer(
    options?: {
      allowHalfOpen?: boolean,
      pauseOnConnect?: boolean,
      ...
    } | connectionListener,
    connectionListener?: connectionListener,
  ): Server;

  declare type connectListener = () => any;
  declare function connect(
    pathOrPortOrOptions:  string | number | net$connectOptions,
    hostOrConnectListener?: string | connectListener,
    connectListener?: connectListener,
  ): Socket;

  declare function createConnection(
    pathOrPortOrOptions:  string | number | net$connectOptions,
    hostOrConnectListener?: string | connectListener,
    connectListener?: connectListener,
  ): Socket;
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
    ...
  },
  ...
};

type os$NetIFAddr = {
  address: string,
  family: string,
  internal: boolean,
  mac: string,
  netmask: string,
  ...
};

type os$UserInfo$buffer = {
  uid: number,
  gid: number,
  username: Buffer,
  homedir: Buffer,
  shell: ?Buffer,
  ...
};

type os$UserInfo$string = {
  uid: number,
  gid: number,
  username: string,
  homedir: string,
  shell: ?string,
  ...
};

declare module "os" {
  declare function arch(): "x64"|"arm"|"ia32";
  declare function cpus(): Array<os$CPU>;
  declare function endianness(): "BE"|"LE";
  declare function freemem(): number;
  declare function homedir(): string;
  declare function hostname(): string;
  declare function loadavg(): [number, number, number];
  declare function networkInterfaces(): { [ifName: string]: Array<os$NetIFAddr>, ... };
  declare function platform(): string;
  declare function release(): string;
  declare function tmpdir(): string;
  declare function totalmem(): number;
  declare function type(): string;
  declare function uptime(): number;
  declare function userInfo(options: { encoding: 'buffer', ... }): os$UserInfo$buffer;
  declare function userInfo(options?: { encoding: 'utf8', ... }): os$UserInfo$string;
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

declare module "punycode" {
  declare function decode(string: string): string;
  declare function encode(string: string): string;
  declare function toASCII(domain: string): string;
  declare function toUnicode(domain: string): string;
  declare var ucs2: {
    decode: (str: string) => Array<number>,
    encode: (codePoints: Array<number>) => string,
    ...
  };
  declare var version : string;
}

declare module "querystring" {
  declare function stringify(
    obj: Object,
    separator?: string,
    equal?: string,
    options?: { encodeURIComponent?: (str: string) => string, ... }
  ): string;
  declare function parse(
    str: string,
    separator: ?string,
    equal: ?string,
    options?: {
      decodeURIComponent?: (str: string) => string,
      maxKeys?: number,
      ...
    }
  ): any;
  declare function escape(str: string): string;
  declare function unescape(str: string, decodeSpaces?: boolean): string;
}

type readline$InterfaceCompleter =
  | (line: string) => [Array<string>, string]
  | (line: string, (err: ?Error, data: [Array<string>, string]) => void) => void;

declare class readline$Interface extends events$EventEmitter {
  close(): void;
  pause(): void;
  prompt(preserveCursor?: boolean): void;
  question(query: string, callback: (answer: string) => void): void;
  resume(): void;
  setPrompt(prompt: string): void;
  write(val: string | void | null, key?: {
    name: string,
    ctrl?: boolean,
    shift?: boolean,
    meta?: boolean,
    ...
  }): void;
  @@asyncIterator(): AsyncIterator<string>;
}

declare module "readline" {
  declare var Interface : typeof readline$Interface;
  declare function clearLine(stream: stream$Stream, dir: -1 | 1 | 0): void;
  declare function clearScreenDown(stream: stream$Stream): void;
  declare function createInterface(opts: {
    input: stream$Readable,
    output?: ?stream$Stream,
    completer?: readline$InterfaceCompleter,
    terminal?: boolean,
    historySize?: number,
    prompt?: string,
    crlfDelay?: number,
    removeHistoryDuplicates?: boolean,
    escapeCodeTimeout?: number,
    ...
  }): readline$Interface;
  declare function cursorTo(stream: stream$Stream, x?: number, y?: number): void;
  declare function moveCursor(stream: stream$Stream, dx: number, dy: number): void;
  declare function emitKeypressEvents(stream: stream$Stream, readlineInterface?: readline$Interface): void;
}

declare class stream$Stream extends events$EventEmitter {}

type readableStreamOptions = {
  highWaterMark?: number,
  encoding?: string,
  objectMode?: boolean,
  read?: (size: number) => void,
  destroy?: (error: ?Error, callback: (error?: Error) => void) => void,
  autoDestroy?: boolean,
  ...
};
declare class stream$Readable extends stream$Stream {
  static from(iterable: Iterable<any> | AsyncIterable<any>, options?: readableStreamOptions): stream$Readable;

  constructor(options?: readableStreamOptions): void;
  destroy(error?: Error): this;
  isPaused(): boolean;
  pause(): this;
  pipe<T: stream$Writable>(dest: T, options?: { end? : boolean, ... }): T;
  read(size?: number): ?(string | Buffer);
  readable: boolean;
  readableHighWaterMark: number;
  readableLength: number;
  resume(): this;
  setEncoding(encoding: string): this;
  unpipe(dest?: stream$Writable): this;
  unshift(chunk: Buffer | Uint8Array | string): void;
  wrap(oldReadable: stream$Stream): this;
  _read(size: number): void;
  _destroy(error: ?Error, callback: (error?: Error) => void): void;
  push(chunk: ?(Buffer | Uint8Array | string), encoding? : string): boolean;
  @@asyncIterator(): AsyncIterator<string | Buffer>;
}

type writableStreamOptions = {
  highWaterMark?: number,
  decodeStrings?: boolean,
  defaultEncoding?: string,
  objectMode?: boolean,
  emitClose?: boolean,
  write?: (chunk: Buffer | string, encoding: string, callback: (error?: Error) => void) => void,
  writev?: (chunks: Array<{
    chunk: Buffer | string,
    encoding: string,
    ...
  }>, callback: (error?: Error) => void) => void,
  destroy?: (error: ?Error, callback: (error?: Error) => void) => void,
  final?: (callback: (error?: Error) => void) => void,
  autoDestroy?: boolean,
  ...
};
declare class stream$Writable extends stream$Stream {
  constructor(options?: writableStreamOptions): void;
  cork(): void;
  destroy(error?: Error): this;
  end(callback?: () => void): this;
  end(chunk?: string | Buffer | Uint8Array, callback?: () => void): this;
  end(chunk?: string | Buffer | Uint8Array, encoding?: string, callback?: () => void): this;
  setDefaultEncoding(encoding: string): this;
  uncork(): void;
  writable: boolean;
  writableHighWaterMark: number;
  writableLength: number;
  write(chunk: string | Buffer | Uint8Array, callback?: (error?: Error) => void): boolean;
  write(chunk: string | Buffer | Uint8Array, encoding?: string,  callback?: (error?: Error) => void): boolean;
  _write(chunk: Buffer | string, encoding: string, callback: (error?: Error) => void): void;
  _writev(chunks: Array<{
    chunk: Buffer | string,
    encoding: string,
    ...
  }>, callback: (error?: Error) => void): void;
  _destroy(error: ?Error, callback: (error?: Error) => void): void;
  _final(callback: (error?: Error) => void): void;
}

//According to the NodeJS docs:
//"Since JavaScript doesn't have multiple prototypal inheritance, this class
//prototypally inherits from Readable, and then parasitically from Writable."
//Source: <https://nodejs.org/api/stream.html#stream_class_stream_duplex_1
type duplexStreamOptions = writableStreamOptions & readableStreamOptions & {
  allowHalfOpen?: boolean,
  readableObjectMode?: boolean,
  writableObjectMode?: boolean,
  readableHighWaterMark?: number,
  writableHighWaterMark?: number,
  ...
};
declare class stream$Duplex extends stream$Readable mixins stream$Writable {
  constructor(options?: duplexStreamOptions): void;
}
type transformStreamOptions = duplexStreamOptions & {
  flush?: (callback: (error: ?Error, data: ?(Buffer | string)) => void) => void,
  transform?: (
    chunk: Buffer | string,
    encoding: string,
    callback: (error: ?Error, data: ?(Buffer | string)) => void,
  ) => void,
  ...
};
declare class stream$Transform extends stream$Duplex {
  constructor(options?: transformStreamOptions): void;
  _flush(callback: (error: ?Error, data: ?(Buffer | string)) => void): void;
  _transform(
    chunk: Buffer | string,
    encoding: string,
    callback: (error: ?Error, data: ?(Buffer | string)) => void
  ): void;
}
declare class stream$PassThrough extends stream$Transform {}

declare module "stream" {
  declare var Stream : typeof stream$Stream
  declare var Readable : typeof stream$Readable
  declare var Writable : typeof stream$Writable
  declare var Duplex : typeof stream$Duplex
  declare var Transform : typeof stream$Transform
  declare var PassThrough : typeof stream$PassThrough
  declare function finished(
    stream: stream$Stream,
    callback: (error?: Error) => void,
  ): () => void;
  declare function finished(
    stream: stream$Stream,
    options: ?{
      error?: boolean,
      readable?: boolean,
      writable?: boolean,
      ...
    },
    callback: (error?: Error) => void,
  ): () => void;
  declare function pipeline<T: stream$Writable>(
    s1: stream$Readable,
    last: T,
    cb: (error?: Error) => void,
  ): T;
  declare function pipeline<T: stream$Writable>(
    s1: stream$Readable,
    s2: stream$Duplex,
    last: T,
    cb: (error?: Error) => void,
  ): T;
  declare function pipeline<T: stream$Writable>(
    s1: stream$Readable,
    s2: stream$Duplex,
    s3: stream$Duplex,
    last: T,
    cb: (error?: Error) => void,
  ): T;
  declare function pipeline<T: stream$Writable>(
    s1: stream$Readable,
    s2: stream$Duplex,
    s3: stream$Duplex,
    s4: stream$Duplex,
    last: T,
    cb: (error?: Error) => void,
  ): T;
  declare function pipeline<T: stream$Writable>(
    s1: stream$Readable,
    s2: stream$Duplex,
    s3: stream$Duplex,
    s4: stream$Duplex,
    s5: stream$Duplex,
    last: T,
    cb: (error?: Error) => void,
  ): T;
  declare function pipeline<T: stream$Writable>(
    s1: stream$Readable,
    s2: stream$Duplex,
    s3: stream$Duplex,
    s4: stream$Duplex,
    s5: stream$Duplex,
    s6: stream$Duplex,
    last: T,
    cb: (error?: Error) => void,
  ): T;
  declare function pipeline(
    streams: Array<stream$Stream>,
    cb: (error?: Error) => void,
  ): stream$Stream;
}

declare class tty$ReadStream extends net$Socket {
  constructor(fd: number, options?: Object): void;
  isRaw : boolean;
  setRawMode(mode : boolean) : void;
  isTTY : true
}
declare class tty$WriteStream extends net$Socket {
  constructor(fd: number) : void;
  /**
   * Clears the current line of this `WriteStream` in a direction identified by `dir`.
   *
   * TODO: takes a callback and returns `boolean` in v12+
   */
  clearLine(dir: -1 | 0 | 1): void;
  columns : number;
  /**
   * Moves this WriteStream's cursor to the specified position
   *
   * TODO: takes a callback and returns `boolean` in v12+
   */
  cursorTo(
    x: number,
    y?: number
  ): void;
  isTTY : true;
  /**
   * Moves this WriteStream's cursor relative to its current position
   *
   * TODO: takes a callback and returns `boolean` in v12+
   */
  moveCursor(dx: number, dy: number): void;
  rows : number;
}

declare module "tty" {
  declare function isatty(fd : number) : boolean;
  declare function setRawMode(mode : boolean) : void;
  declare var ReadStream : typeof tty$ReadStream
  declare var WriteStream : typeof tty$WriteStream
}

declare class string_decoder$StringDecoder {
  constructor(encoding?: 'utf8' | 'ucs2' | 'utf16le' | 'base64'): void;
  end(): string;
  write(buffer: Buffer): string;
}

declare module "string_decoder" {
  declare var StringDecoder : typeof string_decoder$StringDecoder;
}

type tls$connectOptions = {
  port?: number,
  host?: string,
  socket?: net$Socket,
  rejectUnauthorized?: boolean,
  path?: string,
  lookup?: (
    domain: string,
    options?: ?number | ?Object,
    callback?: (err: ?Error, address: string, family: number) => void
  ) => mixed,
  requestOCSP?: boolean,
  ...
};

type tls$Certificate$Subject = {
  C?: string,
  ST?: string,
  L?: string,
  O?: string,
  OU?: string,
  CN?: string,
  ...
};

type tls$Certificate = {
  raw: Buffer,
  subject: tls$Certificate$Subject,
  issuer: tls$Certificate$Subject,
  valid_from: string,
  valid_to: string,
  serialNumber: string,
  fingerprint: string,
  fingerprint256: string,
  ext_key_usage?: Array<string>,
  subjectaltname?: string,
  infoAccess?: { [string]: Array<string>, ... },
  issuerCertificate?: tls$Certificate,
  ...
};

declare class tls$TLSSocket extends net$Socket {
  constructor(socket: net$Socket, options?: Object): void;
  authorized: boolean;
  authorizationError: string | null;
  encrypted: true;
  getCipher(): {
    name: string,
    version: string,
    ...
  } | null;
  getEphemeralKeyInfo(): {
    type: 'DH',
    size: number,
    ...
  } | {
    type: 'EDHC',
    name: string,
    size: number,
    ...
  } | null;
  getPeerCertificate(detailed?: boolean): tls$Certificate | null;
  getSession(): ?Buffer;
  getTLSTicket(): Buffer | void;
  renegotiate(options: Object, callback: Function): boolean | void;
  setMaxSendFragment(size: number): boolean;
}

declare class tls$Server extends net$Server {
  listen(port?: number, hostname?: string, backlog?: number, callback?: Function): tls$Server;
  listen(path: string, callback?: Function): tls$Server;
  listen(handle: Object, callback?: Function): tls$Server;
  close(callback?: Function): tls$Server;
  addContext(hostname: string, context: Object): void;
  getTicketKeys(): Buffer;
  setTicketKeys(keys: Buffer): void;
}

declare module "tls" {
  declare var CLIENT_RENEG_LIMIT: number;
  declare var CLIENT_RENEG_WINDOW: number;
  declare var SLAB_BUFFER_SIZE: number;
  declare var DEFAULT_CIPHERS: string;
  declare var DEFAULT_ECDH_CURVE: string;
  declare function getCiphers(): Array<string>;
  declare function convertNPNProtocols(NPNProtocols: Array<string>, out: Object): void;
  declare function checkServerIdentity(servername: string, cert: string): Error | void;
  declare function parseCertString(s: string): Object;
  declare function createSecureContext(details: Object): Object;
  declare var SecureContext: Object;
  declare var TLSSocket: typeof tls$TLSSocket;
  declare var Server: typeof tls$Server;
  declare function createServer(options: Object, secureConnectionListener?: Function): tls$Server;
  declare function connect(options: tls$connectOptions, callback?: Function): tls$TLSSocket;
  declare function connect(port: number, host?: string, options?: tls$connectOptions, callback?: Function): tls$TLSSocket;
  declare function createSecurePair(context?: Object, isServer?: boolean, requestCert?: boolean, rejectUnauthorized?: boolean, options?: Object): Object;
}

type url$urlObject = {
  +href?: string,
  +protocol?: string | null,
  +slashes?: boolean | null,
  +auth?: string | null,
  +hostname?: string | null,
  +port?: string | number | null,
  +host?: string | null,
  +pathname?: string | null,
  +search?: string | null,
  +query?: Object | null,
  +hash?: string | null,
  ...
};

declare module "url" {
  declare type Url = {|
    protocol: string | null,
    slashes: boolean | null,
    auth: string | null,
    host: string | null,
    port: string | null,
    hostname: string | null,
    hash: string | null,
    search: string | null,
    query: string | null | { [string]: string, ... },
    pathname: string | null,
    path: string | null,
    href: string,
  |}

  declare type UrlWithStringQuery = {|
    ...Url,
    query: string | null
  |}

  declare type UrlWithParsedQuery = {|
    ...Url,
    query: { [string]: string, ... }
  |}

  declare function parse(urlStr: string, parseQueryString: true, slashesDenoteHost?: boolean): UrlWithParsedQuery;
  declare function parse(urlStr: string, parseQueryString?: false | void, slashesDenoteHost?: boolean): UrlWithStringQuery;
  declare function parse(urlStr: string, parseQueryString?: boolean, slashesDenoteHost?: boolean): Url;
  declare function format(urlObj: url$urlObject): string;
  declare function resolve(from: string, to: string): string;
  declare function domainToASCII(domain: string): string;
  declare function domainToUnicode(domain: string): string;
  declare function pathToFileURL(path: string): url$urlObject;
  declare function fileURLToPath(path: url$urlObject | string): string;
  declare class URLSearchParams {
    constructor(init?: string | Array<[string, string]> | { [string]: string, ... } ): void;
    append(name: string, value: string): void;
    delete(name: string): void;
    entries(): Iterator<[string, string]>;
    forEach<This>(fn: (this : This, value: string, name: string, searchParams: URLSearchParams) => void, thisArg?: This): void;
    get(name: string): string | null;
    getAll(name: string): string[];
    has(name: string): boolean;
    keys(): Iterator<string>;
    set(name: string, value: string): void;
    sort(): void;
    toString(): string;
    values(): Iterator<string>;
    @@iterator(): Iterator<[string, string]>;
  }
  declare class URL {
    constructor(input: string, base?: string | URL): void;
    hash: string;
    host: string;
    hostname: string;
    href: string;
    origin: string;
    password: string;
    pathname: string;
    port: string;
    protocol: string;
    search: string;
    +searchParams: URLSearchParams;
    username: string;
    toString(): string;
    toJSON(): string;
  }
}

type util$InspectOptions = {
  showHidden?: boolean,
  depth?: ?number,
  colors?: boolean,
  customInspect?: boolean,
  ...
};

declare module "util" {
  declare function debuglog(section: string): (data: any, ...args: any) => void;
  declare function format(format: string, ...placeholders: any): string;
  declare function log(string: string): void;
  declare function inspect(object: any, options?: util$InspectOptions): string;
  declare function isArray(object: any): boolean;
  declare function isRegExp(object: any): boolean;
  declare function isDate(object: any): boolean;
  declare function isError(object: any): boolean;
  declare function inherits(constructor: Function, superConstructor: Function): void;
  declare function deprecate(f: Function, string: string): Function;
  declare function promisify(f: Function): Function;
  declare function callbackify(f: Function): Function;

  declare class TextDecoder {
    constructor(encoding?: string, options: {
      fatal?: boolean,
      ignoreBOM?: boolean,
      ...
    }): void;
    decode(input?: ArrayBuffer | DataView | $TypedArray, options?: { stream?: boolean, ... }): string;
    encoding: string;
    fatal: boolean;
    ignoreBOM: boolean;
  }

  declare class TextEncoder {
    constructor(): void;
    encode(input?: string): Uint8Array;
    encoding: string;
  }
}

type vm$ScriptOptions = {
  cachedData?: Buffer,
  columnOffset?: number,
  displayErrors?: boolean,
  filename?: string,
  lineOffset?: number,
  produceCachedData?: boolean,
  timeout?: number,
  ...
};

type vm$CreateContextOptions = {
  name?: string;
  origin?: string;
  codeGeneration?: {
    strings?: boolean;
    wasm?: boolean;
    ...
  };
  ...
}

type vm$CompileFunctionOptions = {
  filename?: string;
  lineOffset?: number;
  columnOffset?: number;
  cachedData?: Buffer;
  produceCachedData?: boolean;
  parsingContext?: { [key: string]: any, ... };
  contextExtensions?: Array<{ [key: string]: any, ... }>;
  ...
}

declare class vm$Script {
  constructor(code: string, options?: vm$ScriptOptions | string): void;
  cachedData: ?Buffer;
  cachedDataRejected: ?boolean;
  cachedDataProduced: ?boolean;
  runInContext(contextifiedSandbox: vm$Context, options?: vm$ScriptOptions): any;
  runInNewContext(sandbox?: { [key: string]: any, ... }, options?: vm$ScriptOptions): any;
  runInThisContext(options?: vm$ScriptOptions): any;
  createCachedData(): Buffer;
}

declare class vm$Context {}

declare module "vm" {
  declare var Script: typeof vm$Script;
  declare function createContext(sandbox?: interface { [key: string]: any }, options?: vm$CreateContextOptions): vm$Context;
  declare function isContext(sandbox: { [key: string]: any, ... }): boolean;
  declare function runInContext(code: string, contextifiedSandbox: vm$Context, options?: vm$ScriptOptions | string): any;
  declare function runInDebugContext(code: string): any;
  declare function runInNewContext(code: string, sandbox?: { [key: string]: any, ... }, options?: vm$ScriptOptions | string): any;
  declare function runInThisContext(code: string, options?: vm$ScriptOptions | string): any;
  declare function compileFunction(code: string, params: string[], options: vm$CompileFunctionOptions): Function;
}

type zlib$options = {
  flush?: number,
  chunkSize?: number,
  windowBits?: number,
  level?: number,
  memLevel?: number,
  strategy?: number,
  dictionary?: Buffer,
  ...
};

type zlib$brotliOptions = {
  flush?: number,
  finishFlush?: number,
  chunkSize?: number,
  params?: {
    [number]: boolean | number,
    ...
  },
  maxOutputLength?: number,
  ...
}

type zlib$syncFn = (
  buffer: Buffer | $TypedArray | DataView | ArrayBuffer | string,
  options?: zlib$options
) => Buffer;

type zlib$asyncFn = (
  buffer: Buffer | $TypedArray | DataView | ArrayBuffer | string,
  options?: zlib$options,
  callback?: (error: ?Error, result: Buffer) => void
) => void;

type zlib$brotliSyncFn = (
  buffer: Buffer | $TypedArray | DataView | ArrayBuffer | string,
  options?: zlib$brotliOptions
) => Buffer;

type zlib$brotliAsyncFn = (
  buffer: Buffer | $TypedArray | DataView | ArrayBuffer | string,
  options?: zlib$brotliOptions,
  callback?: (error: ?Error, result: Buffer) => void
) => void;

// Accessing the constants directly from the module is currently still
// possible but should be considered deprecated.
// ref: https://github.com/nodejs/node/blob/master/doc/api/zlib.md
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
  declare var constants: {
    Z_NO_FLUSH: number,
    Z_PARTIAL_FLUSH: number,
    Z_SYNC_FLUSH: number,
    Z_FULL_FLUSH: number,
    Z_FINISH: number,
    Z_BLOCK: number,
    Z_TREES: number,
    Z_OK: number,
    Z_STREAM_END: number,
    Z_NEED_DICT: number,
    Z_ERRNO: number,
    Z_STREAM_ERROR: number,
    Z_DATA_ERROR: number,
    Z_MEM_ERROR: number,
    Z_BUF_ERROR: number,
    Z_VERSION_ERROR: number,
    Z_NO_COMPRESSION: number,
    Z_BEST_SPEED: number,
    Z_BEST_COMPRESSION: number,
    Z_DEFAULT_COMPRESSION: number,
    Z_FILTERED: number,
    Z_HUFFMAN_ONLY: number,
    Z_RLE: number,
    Z_FIXED: number,
    Z_DEFAULT_STRATEGY: number,
    Z_BINARY: number,
    Z_TEXT: number,
    Z_ASCII: number,
    Z_UNKNOWN: number,
    Z_DEFLATED: number,
    Z_NULL: number,
    Z_DEFAULT_CHUNK: number,
    Z_DEFAULT_LEVEL: number,
    Z_DEFAULT_MEMLEVEL: number,
    Z_DEFAULT_WINDOWBITS: number,
    Z_MAX_CHUNK: number,
    Z_MAX_LEVEL: number,
    Z_MAX_MEMLEVEL: number,
    Z_MAX_WINDOWBITS: number,
    Z_MIN_CHUNK: number,
    Z_MIN_LEVEL: number,
    Z_MIN_MEMLEVEL: number,
    Z_MIN_WINDOWBITS: number,

    BROTLI_DECODE: number,
    BROTLI_ENCODE: number,
    BROTLI_OPERATION_PROCESS: number,
    BROTLI_OPERATION_FLUSH: number,
    BROTLI_OPERATION_FINISH: number,
    BROTLI_OPERATION_EMIT_METADATA: number,
    BROTLI_PARAM_MODE: number,
    BROTLI_MODE_GENERIC: number,
    BROTLI_MODE_TEXT: number,
    BROTLI_MODE_FONT: number,
    BROTLI_DEFAULT_MODE: number,
    BROTLI_PARAM_QUALITY: number,
    BROTLI_MIN_QUALITY: number,
    BROTLI_MAX_QUALITY: number,
    BROTLI_DEFAULT_QUALITY: number,
    BROTLI_PARAM_LGWIN: number,
    BROTLI_MIN_WINDOW_BITS: number,
    BROTLI_MAX_WINDOW_BITS: number,
    BROTLI_LARGE_MAX_WINDOW_BITS: number,
    BROTLI_DEFAULT_WINDOW: number,
    BROTLI_PARAM_LGBLOCK: number,
    BROTLI_MIN_INPUT_BLOCK_BITS: number,
    BROTLI_MAX_INPUT_BLOCK_BITS: number,
    BROTLI_PARAM_DISABLE_LITERAL_CONTEXT_MODELING: number,
    BROTLI_PARAM_SIZE_HINT: number,
    BROTLI_PARAM_LARGE_WINDOW: number,
    BROTLI_PARAM_NPOSTFIX: number,
    BROTLI_PARAM_NDIRECT: number,
    BROTLI_DECODER_RESULT_ERROR: number,
    BROTLI_DECODER_RESULT_SUCCESS: number,
    BROTLI_DECODER_RESULT_NEEDS_MORE_INPUT: number,
    BROTLI_DECODER_RESULT_NEEDS_MORE_OUTPUT: number,
    BROTLI_DECODER_PARAM_DISABLE_RING_BUFFER_REALLOCATION: number,
    BROTLI_DECODER_PARAM_LARGE_WINDOW: number,
    BROTLI_DECODER_NO_ERROR: number,
    BROTLI_DECODER_SUCCESS: number,
    BROTLI_DECODER_NEEDS_MORE_INPUT: number,
    BROTLI_DECODER_NEEDS_MORE_OUTPUT: number,
    BROTLI_DECODER_ERROR_FORMAT_EXUBERANT_NIBBLE: number,
    BROTLI_DECODER_ERROR_FORMAT_RESERVED: number,
    BROTLI_DECODER_ERROR_FORMAT_EXUBERANT_META_NIBBLE: number,
    BROTLI_DECODER_ERROR_FORMAT_SIMPLE_HUFFMAN_ALPHABET: number,
    BROTLI_DECODER_ERROR_FORMAT_SIMPLE_HUFFMAN_SAME: number,
    BROTLI_DECODER_ERROR_FORMAT_CL_SPACE: number,
    BROTLI_DECODER_ERROR_FORMAT_HUFFMAN_SPACE: number,
    BROTLI_DECODER_ERROR_FORMAT_CONTEXT_MAP_REPEAT: number,
    BROTLI_DECODER_ERROR_FORMAT_BLOCK_LENGTH_1: number,
    BROTLI_DECODER_ERROR_FORMAT_BLOCK_LENGTH_2: number,
    BROTLI_DECODER_ERROR_FORMAT_TRANSFORM: number,
    BROTLI_DECODER_ERROR_FORMAT_DICTIONARY: number,
    BROTLI_DECODER_ERROR_FORMAT_WINDOW_BITS: number,
    BROTLI_DECODER_ERROR_FORMAT_PADDING_1: number,
    BROTLI_DECODER_ERROR_FORMAT_PADDING_2: number,
    BROTLI_DECODER_ERROR_FORMAT_DISTANCE: number,
    BROTLI_DECODER_ERROR_DICTIONARY_NOT_SET: number,
    BROTLI_DECODER_ERROR_INVALID_ARGUMENTS: number,
    BROTLI_DECODER_ERROR_ALLOC_CONTEXT_MODES: number,
    BROTLI_DECODER_ERROR_ALLOC_TREE_GROUPS: number,
    BROTLI_DECODER_ERROR_ALLOC_CONTEXT_MAP: number,
    BROTLI_DECODER_ERROR_ALLOC_RING_BUFFER_1: number,
    BROTLI_DECODER_ERROR_ALLOC_RING_BUFFER_2: number,
    BROTLI_DECODER_ERROR_ALLOC_BLOCK_TYPE_TREES: number,
    BROTLI_DECODER_ERROR_UNREACHABL: number,
    ...
  };
  declare var codes: {
    Z_OK: number,
    Z_STREAM_END: number,
    Z_NEED_DICT: number,
    Z_ERRNO: number,
    Z_STREAM_ERROR: number,
    Z_DATA_ERROR: number,
    Z_MEM_ERROR: number,
    Z_BUF_ERROR: number,
    Z_VERSION_ERROR: number,
    ...
  };
  declare class Zlib extends stream$Duplex {
    // TODO
  }
  declare class BrotliCompress extends Zlib {}
  declare class BrotliDecompress extends Zlib {}
  declare class Deflate extends Zlib {}
  declare class Inflate extends Zlib {}
  declare class Gzip extends Zlib {}
  declare class Gunzip extends Zlib {}
  declare class DeflateRaw extends Zlib {}
  declare class InflateRaw extends Zlib {}
  declare class Unzip extends Zlib {}
  declare function createBrotliCompress(options?: zlib$brotliOptions): BrotliCompress;
  declare function createBrotliDecompress(options?: zlib$brotliOptions): BrotliDecompress;
  declare function createDeflate(options?: zlib$options): Deflate;
  declare function createInflate(options?: zlib$options): Inflate;
  declare function createDeflateRaw(options?: zlib$options): DeflateRaw;
  declare function createInflateRaw(options?: zlib$options): InflateRaw;
  declare function createGzip(options?: zlib$options): Gzip;
  declare function createGunzip(options?: zlib$options): Gunzip;
  declare function createUnzip(options?: zlib$options): Unzip;
  declare var brotliCompress: zlib$brotliAsyncFn;
  declare var brotliCompressSync: zlib$brotliSyncFn;
  declare var brotliDeompress: zlib$brotliAsyncFn;
  declare var brotliDecompressSync: zlib$brotliSyncFn;
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
  declare class AssertionError extends Error {}
  declare type AssertStrict = {
    (value: any, message?: string): void;
    ok(value: any, message?: string): void;
    fail(message?: string | Error): void;
    // deprecated since v10.15
    fail(actual: any, expected: any, message: string, operator: string): void;
    equal(actual: any, expected: any, message?: string): void;
    notEqual(actual: any, expected: any, message?: string): void;
    deepEqual(actual: any, expected: any, message?: string): void;
    notDeepEqual(actual: any, expected: any, message?: string): void;
    throws(
      block: Function,
      error?: Function | RegExp | (err: any) => boolean,
      message?: string
    ): void;
    doesNotThrow(block: Function, message?: string): void;
    ifError(value: any): void;
    AssertionError: typeof AssertionError;
    strict: AssertStrict;
    ...
  }
  declare module.exports: {
    (value: any, message?: string): void,
    ok(value: any, message?: string): void,
    fail(message?: string | Error): void,
    // deprecated since v10.15
    fail(actual: any, expected: any, message: string, operator: string): void,
    equal(actual: any, expected: any, message?: string): void,
    notEqual(actual: any, expected: any, message?: string): void,
    deepEqual(actual: any, expected: any, message?: string): void,
    notDeepEqual(actual: any, expected: any, message?: string): void,
    strictEqual(actual: any, expected: any, message?: string): void,
    notStrictEqual(actual: any, expected: any, message?: string): void,
    deepStrictEqual(actual: any, expected: any, message?: string): void,
    notDeepStrictEqual(actual: any, expected: any, message?: string): void,
    throws(
      block: Function,
      error?: Function | RegExp | (err: any) => boolean,
      message?: string
    ): void,
    doesNotThrow(block: Function, message?: string): void,
    ifError(value: any): void,
    AssertionError: typeof AssertionError,
    strict: AssertStrict;
    ...
  }
}

type HeapStatistics = {
  total_heap_size: number,
  total_heap_size_executable: number,
  total_physical_size: number,
  total_available_size: number,
  used_heap_size: number,
  heap_size_limit: number,
  malloced_memory: number,
  peak_malloced_memory: number,
  does_zap_garbage: number,
  ...
}

type HeapSpaceStatistics = {
  space_name: string,
  space_size: number,
  space_used_size: number,
  space_available_size: number,
  physical_space_size: number,
  ...
}

declare module "v8" {
  declare function getHeapStatistics() : HeapStatistics;
  declare function getHeapSpaceStatistics() : Array<HeapSpaceStatistics>
  declare function setFlagsFromString(flags: string) : void;
}

type repl$DefineCommandOptions =
  | (...args: Array<any>) => void
  | {
  action: (...args: Array<any>) => void,
  help?: string,
  ...
};

declare class $SymbolReplModeMagic mixins Symbol {}
declare class $SymbolReplModeSloppy mixins Symbol {}
declare class $SymbolReplModeStrict mixins Symbol {}

declare module 'repl' {
  declare var REPL_MODE_MAGIC: $SymbolReplModeMagic;
  declare var REPL_MODE_SLOPPY: $SymbolReplModeSloppy;
  declare var REPL_MODE_STRICT: $SymbolReplModeStrict;

  declare class REPLServer extends readline$Interface {
    context: vm$Context;
    defineCommand(command: string, options: repl$DefineCommandOptions): void;
    displayPrompt(preserveCursor?: boolean): void;
  }

  declare function start(prompt: string): REPLServer;
  declare function start(options: {
    prompt?: string,
    input?: stream$Readable,
    output?: stream$Writable,
    terminal?: boolean,
    eval?: Function,
    useColors?: boolean,
    useGlobal?: boolean,
    ignoreUndefined?: boolean,
    writer?: (object: any, options?: util$InspectOptions) => string,
    completer?: readline$InterfaceCompleter,
    replMode?: $SymbolReplModeMagic | $SymbolReplModeSloppy | $SymbolReplModeStrict,
    breakEvalOnSigint?: boolean,
    ...
  }): REPLServer;

  declare class Recoverable extends SyntaxError {
    constructor(err: Error): void;
  }
}

/* globals: https://nodejs.org/api/globals.html */

type process$CPUUsage = {
  user: number,
  system: number,
  ...
}

declare class Process extends events$EventEmitter {
  abort() : void;
  allowedNodeEnvironmentFlags: Set<string>;
  arch : string;
  argv : Array<string>;
  chdir(directory : string) : void;
  config : Object;
  connected : boolean;
  cpuUsage(previousValue?: process$CPUUsage) : process$CPUUsage;
  cwd() : string;
  disconnect? : () => void;
  domain? : domain$Domain;
  env : { [key: string] : string | void, ... };
  emitWarning(warning: string | Error): void;
  emitWarning(warning: string, typeOrCtor: string | (...empty) => mixed): void;
  emitWarning(warning: string, type: string, codeOrCtor: string | (...empty) => mixed): void;
  emitWarning(
    warning: string,
    type: string,
    code: string,
    ctor?: (...empty) => mixed
  ): void;
  execArgv : Array<string>;
  execPath : string;
  exit(code? : number) : empty;
  exitCode? : number;
  getegid? : () => number;
  geteuid? : () => number;
  getgid? : () => number;
  getgroups? : () => Array<number>;
  getuid? : () => number;
  hrtime(time?: [ number, number ]) : [ number, number ];
  initgroups? : (user : number | string, extra_group : number | string) => void;
  kill(pid : number, signal? : string | number) : void;
  mainModule : Object;
  memoryUsage() : {
    rss : number,
    heapTotal : number,
    heapUsed : number,
    external : number,
    ...
  };
  nextTick: <T>(cb: (...T) => mixed, ...T) => void;
  pid : number;
  platform : string;
  release : {
    name : string,
    lts? : string,
    sourceUrl : string,
    headersUrl : string,
    libUrl : string,
    ...
  };
  send? : (message : any,
           sendHandleOrCallback? : net$Socket | net$Server | Function,
           callback? : Function) => void;
  setegid? : (id : number | string) => void;
  seteuid? : (id : number | string) => void;
  setgid? : (id : number | string) => void;
  setgroups? : <Group: string | number>(groups : Array<Group>) => void;
  setuid? : (id : number | string) => void;
  stderr : stream$Writable | tty$WriteStream;
  stdin : stream$Readable | tty$ReadStream;
  stdout : stream$Writable | tty$WriteStream;
  title : string;
  umask(mask? : number) : number;
  uptime() : number;
  version : string;
  versions : {
    [key: string] : ?string,
    node : string,
    v8 : string,
    ...
  };
}
declare var process: Process;

declare var __filename: string;
declare var __dirname: string;

declare function setImmediate(callback: ((...args: Array<any>) => mixed), ...args: Array<any>): Object;
declare function clearImmediate(immediateObject: any): Object;
