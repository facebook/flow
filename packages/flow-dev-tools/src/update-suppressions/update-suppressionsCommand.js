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

class UpdateSuppressionsCommand extends Base<{}> {
  static async go(): Promise<void> {
    process.stderr.write('use `flow dev-tools update-suppressions` instead\n');
    process.exit(this.BAD_ARGS);
  }

  static description(): string {
    return 'use `flow dev-tools update-suppressions` instead';
  }
}

module.exports = {
  default: UpdateSuppressionsCommand,
};
