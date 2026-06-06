/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

'use strict';

/**
 * Decode a UTF-8 encoded string from Hermes with a known length.
 * Based on Emscripten's UTF8ToString with the following differences:
 * - Always reads all bytes up to the given length, including null bytes. This
 *   means that we can decode strings that contain null bytes in the middle.
 * - Allow UTF-8 encoded code points that are part of a surrogate pair, even though
 *   this is technically invalid UTF-8 that UTF8ToString would convert to 0xfffd.
 */
export default function HermesParserDecodeUTF8String(
  ptrIn: number,
  length: number,
  heap: Uint8Array,
): string {
  let ptr = ptrIn;
  const endPtr = ptr + length;
  let str = '';

  while (ptr < endPtr) {
    // ASCII characters fit in single byte code point
    let u0 = heap[ptr++];
    if (!(u0 & 0x80)) {
      str += String.fromCharCode(u0);
      continue;
    }

    // Two byte code point
    const u1 = heap[ptr++] & 0x3f;
    if ((u0 & 0xe0) === 0xc0) {
      str += String.fromCharCode(((u0 & 0x1f) << 6) | u1);
      continue;
    }

    const u2 = heap[ptr++] & 0x3f;
    if ((u0 & 0xf0) === 0xe0) {
      // Three byte code point
      u0 = ((u0 & 0x0f) << 12) | (u1 << 6) | u2;
    } else {
      // Four byte code point
      u0 = ((u0 & 0x07) << 18) | (u1 << 12) | (u2 << 6) | (heap[ptr++] & 0x3f);
    }

    if (u0 < 0x10000) {
      // Code point fits into a single UTF-16 code unit
      str += String.fromCharCode(u0);
    } else {
      // Code point does not fit into single UTF-16 code unit so convert to surrogate pair
      u0 -= 0x10000;
      str += String.fromCharCode(0xd800 | (u0 >> 10), 0xdc00 | (u0 & 0x3ff));
    }
  }

  return str;
}
