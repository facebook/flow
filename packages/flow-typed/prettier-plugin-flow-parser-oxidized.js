/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow strict
 * @format
 */

declare module 'prettier-plugin-flow-parser-oxidized' {
  import type {Parser, Printer} from 'prettier';

  declare export var parsers: {[string]: Parser<>, ...};
  declare export var printers: {[string]: Printer<>, ...};
  declare export var languages: $ReadOnlyArray<mixed>;
  declare export var options: {[string]: mixed, ...};
}
