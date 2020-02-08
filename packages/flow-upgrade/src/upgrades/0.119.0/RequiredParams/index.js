/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

const path = require('path');
const Styled = require('../../../Styled');

exports.kind = 'codemod';

exports.title = 'Makes function parameters optional.';

exports.description = `
TODO
`;
exports.transformPath = path.join(__dirname, './codemod.js');
