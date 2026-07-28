/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

const babel = require('@babel/core');
const Module = require('module');

const srcDir = require('path').resolve(__dirname, '..');

const originalCompile = Module.prototype._compile;
Module.prototype._compile = function (code, filename) {
  if (filename.startsWith(srcDir) && filename.endsWith('.js')) {
    const result = babel.transformSync(code, {
      filename,
      presets: ['@babel/preset-flow'],
      plugins: ['babel-plugin-syntax-hermes-parser'],
    });
    code = result.code;
  }
  return originalCompile.call(this, code, filename);
};

const {exec} = require('../utils/async');
const {splitIntoChunks} = require('../utils/string');

class Expected {
  constructor(actualValue) {
    this.actualValue = actualValue;
  }

  toBe(expectedValue) {
    if (this.actualValue !== expectedValue) {
      throw new Error(`Expected ${this.actualValue} to be ${expectedValue}`);
    }
  }

  toEqual(expectedValue) {
    const actual = JSON.stringify(this.actualValue);
    const expected = JSON.stringify(expectedValue);
    if (actual !== expected) {
      throw new Error(`Expected ${actual} to be ${expected}`);
    }
  }
}

function expect(v) {
  return new Expected(v);
}

function repeatString(str, times) {
  let result = '';
  for (let i = 0; i < times; i++) {
    result += str;
  }
  return result;
}

const collectedTests = [];

function test(name, fn) {
  collectedTests.push({name, fn});
}

test('exec', async () => {
  expect(await exec('echo foo')).toBe('foo\n');
  expect(await exec('cat', {stdin: 'bar'})).toBe('bar');

  expect(repeatString('foo', 2)).toBe('foofoo');

  // make the string big enough that it exceeds the chunk size for writes
  const repeatedString = repeatString('0123456789', 2000);
  expect(await exec('cat', {stdin: repeatedString})).toBe(repeatedString);
});

test('splitIntoChunks', () => {
  expect(splitIntoChunks('', 1)).toEqual([]);
  expect(splitIntoChunks('abcd', 2)).toEqual(['ab', 'cd']);
  expect(splitIntoChunks('abc', 2)).toEqual(['ab', 'c']);
  expect(splitIntoChunks('abc', 10)).toEqual(['abc']);
  expect(splitIntoChunks('abc', 1)).toEqual(['a', 'b', 'c']);
  // The check marks are multi-byte characters when encoded with UTF-8. Make sure they are treated
  // as single characters and not split up into individual bytes.
  expect(splitIntoChunks('✓✓✓✓✓', 1)).toEqual(['✓', '✓', '✓', '✓', '✓']);
});

(async () => {
  for (const {name, fn} of collectedTests) {
    try {
      await fn();
      console.error(`[SUCCESS] ${name}`);
    } catch (e) {
      console.error(`[FAILURE] ${name}`);
      console.error(e);
      throw e;
    }
  }
})();
