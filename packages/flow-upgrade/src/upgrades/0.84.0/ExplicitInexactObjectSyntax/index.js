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

exports.title = 'Adds `...` to the end of all inexact object types.';

exports.description = `
Flow is changing its object type syntax to be exact by default. See the blog
post at https://medium.com/flow-type/on-the-roadmap-exact-objects-by-default-16b72933c5cf
for details. This codemod will add '...' to the end of all inexact object types.
`;
exports.transformPath = path.join(__dirname, './codemod.js');
