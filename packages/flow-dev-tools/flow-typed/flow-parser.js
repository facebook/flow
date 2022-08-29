/**
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

// TODO. flow-parser doesn't ship with types anyway :(
// if it did, we'd have to figure out how to keep it up to date
// at dev time since it's in the same yarn workspace. but for now,
// this stub prevents having to compile flow-parser to `flow check`
// flow-dev-tools.
declare module 'flow-parser' {
  declare module.exports: any;
}
