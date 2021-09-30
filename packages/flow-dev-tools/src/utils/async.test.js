/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

const {exec} = require('./async');

function repeatString(str, times) {
  let result = '';
  for (let i = 0; i < times; i++) {
    result += str;
  }
  return result;
}

test('exec', async () => {
  expect(await exec('echo foo')).toBe('foo\n');
  expect(await exec('cat', {stdin: 'bar'})).toBe('bar');

  expect(repeatString('foo', 2)).toBe('foofoo');

  // make the string big enough that it exceeds the chunk size for writes
  const repeatedString = repeatString('0123456789', 2000);
  expect(await exec('cat', {stdin: repeatedString})).toBe(repeatedString);
});
