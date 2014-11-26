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
  stdio: string|Array;
  uid: number;
}

type child_process$ChildProcess = {
  stdin: Stream;
  stdout: Stream;
  stderr: Stream;
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

declare module "dgram" {
}

declare module "dns" {
  // TODO
}

declare module "domain" {
  // TODO
}

declare module "events" {
  // TODO
}

declare module "fs" {
  // TODO
}

declare module "http" {
  // TODO
}

declare module "https" {
  // TODO
}

declare module "net" {
  // TODO
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

type Stream = {
  // TODO
}

declare module "stream" {
  // TODO
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
  // TODO
}

declare module "vm" {
  // TODO
}

declare module "zlib" {
  // TODO
}
