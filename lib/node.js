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
  constructor(value: Array<number> | number | string, encoding?: string): void;
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

type child_process$execFileOpts = {
  cwd: string;
  env: Object;
  encoding: string;
  timeout: number;
  maxBuffer: number;
  killSignal: string;
};

type child_process$execOpts = {
  cwd: ?string;
  env: ?Object;
  encoding: ?string;
  timeout: ?number;
  maxBuffer: ?number;
  killSignal: ?string;
};

type child_process$forkOpts = {
  cwd: string;
  encoding: string;
  env: Object;
  execArgv: Array<string>;
  execPath: string;
  silent: boolean;
};

type child_process$Handle = any; // TODO

type child_process$spawnOpts = {
  cwd: string;
  detached: boolean;
  env: Object;
  gid: number;
  stdio: string|Array<any>;
  uid: number;
}

type child_process$ChildProcess = {
  stdin: stream$Stream;
  stdout: stream$Stream;
  stderr: stream$Stream;
  pid: number;
  connected: boolean;

  disconnect(): void;
  kill(signal?: string): void;
  send(message: Object, sendHandle?: child_process$Handle): void;
}

declare module "child_process" {
  declare function exec(
    command: string,
    options?: Object, // TODO: child_process$execOpts,
    callback: (error: ?Error, stdout: Buffer, stderr: Buffer) => void
  ): child_process$ChildProcess;

  declare function execFile(
    file: string,
    args?: Array<string>,
    options: child_process$execFileOpts,
    callback: (error: ?Error, stdout: Buffer, stderr: Buffer) => void
  ): child_process$ChildProcess;

  declare function fork(
    modulePath: string,
    args?: Array<string>,
    options?: child_process$forkOpts
  ): child_process$ChildProcess;

  declare function spawn(
    command: string,
    args?: Array<string>,
    options?: child_process$spawnOpts
  ): child_process$ChildProcess;
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

  declare function createCipher(algorithm: string, password: string): crypto$Cipher;
  declare function createCipheriv(
    algorithm: string,
    key: string,
    iv: string
  ): crypto$Cipher;
  declare function createCredentials(
    details?: crypto$createCredentialsDetails
  ): crypto$Credentials
  declare function createDecipher(algorithm: string, password: string): crypto$Decipher;
  declare function createDecipheriv(
    algorithm: string,
    key: string,
    iv: string
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
  on(event: string, listener: (data: any) => void): events$EventEmitter;
  once(event: string, listener: Function): events$EventEmitter;
  removeAllListeners(events?: Array<string>): events$EventEmitter;
  removeListener(event: string, listener: Function): events$EventEmitter;
  setMaxListeners(n: number): void;
}

declare module "events" {
  // TODO: See the comment above the events$EventEmitter declaration
  declare class EventEmitter extends events$EventEmitter {}
}

declare module "fs" {
  // TODO
}

declare module "http" {
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

  declare function request(
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

declare module "os" {
  // TODO
}

declare module "path" {
  // TODO
}

declare module "punycode" {
  // TODO
}

declare module "querystring" {
  // TODO
}

declare module "readline" {
  // TODO
}

declare class stream$Stream extends events$EventEmitter {
  pipe(dest: any, options: any): void;
}

declare class stream$Readable extends stream$Stream {
  setEncoding(): void;
}

declare class stream$Writable extends stream$Stream {
  close(): void;
  finish(): void;
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
  // TODO
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

declare module "zlib" {
  // TODO
}

declare module "assert" {
  declare function fail(actual: any, expected: any, message: string, operator: string): void;
  declare function ok(value: any, message?: string): void;
  declare function equal(actual: any, expected: any, message?: string): void;
  declare function notEqual(actual: any, expected: any, message?: string): void;
  declare function deepEqual(actual: any, expected: any, message?: string): void;
  declare function notDeepEqual(actual: any, expected: any, message?: string): void;
  declare function strictEqual(actual: any, expected: any, message?: string): void;
  declare function notStrictEqual(actual: any, expected: any, message?: string): void;
  declare function throws(
    block: Function,
    error?: Function | RegExp | (err: any) => boolean,
    message?: string
  ): void;
  declare function doesNotThrow(block: Function, message?: string): void;
  declare function ifError(value: any): void;
}
