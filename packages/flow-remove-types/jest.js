/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

var flowRemoveTypes = require('./index');

module.exports = {
  process: function(src, filename) {
    return flowRemoveTypes(src).toString();
  }
};
