/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @flow
 * @format
 */

const {default: Base} = require('../command/Base');

const MESSAGE =
  'The `add-comments` command has been removed from flow-dev-tools. ' +
  'Use `flow dev-tools add-comments` instead.\n';

class AddCommentsCommand extends Base<Object> {
  static async go(): Promise<void> {
    process.stderr.write(MESSAGE);
    process.exit(1);
  }

  static description(): string {
    return 'Removed; use `flow dev-tools add-comments` instead';
  }

  static async usage(): Promise<string> {
    return MESSAGE;
  }
}

module.exports = {default: AddCommentsCommand};
