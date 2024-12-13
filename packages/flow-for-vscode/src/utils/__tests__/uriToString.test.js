/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import { URI } from 'vscode-uri';
import uriToString from '../uriToString';

jest.mock('../isWindows', () => {
  return () => true;
});

test('[windows] lower-case drive letter', () => {
  const uri = URI.file('c:/flow/test');
  expect(uriToString(uri)).toEqual('file:///C:/flow/test');
});

test('[windows] upper-case drive letter', () => {
  const uri = URI.file('C:/flow/test');
  expect(uriToString(uri)).toEqual('file:///C:/flow/test');
});

test('[windows] upper-case drive letter', () => {
  const uri = URI.file('e:/flow/test');
  expect(uriToString(uri)).toEqual('file:///E:/flow/test');
});

test('[windows] upper-case drive letter', () => {
  const uri = URI.file('/e:/flow/test');
  expect(uriToString(uri)).toEqual('file:///E:/flow/test');
});

test('linux path', () => {
  const uri = URI.file('/flow/test');
  expect(uriToString(uri)).toEqual('file:///flow/test');
});
