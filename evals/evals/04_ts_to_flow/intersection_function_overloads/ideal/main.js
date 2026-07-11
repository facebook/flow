/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 */

type Convert = ((input: number) => string) & ((input: string) => number);

declare const convert: Convert;

interface Reader {
  read(): string;
}

interface Writer {
  write(data: string): void;
}

type ReadWriter = Reader & Writer;

function copy(rw: ReadWriter): void {
  rw.write(rw.read());
}

const encoded: string = convert(10);
const decoded: number = convert('42');

declare const device: ReadWriter;
copy(device);

console.log(encoded, decoded);
