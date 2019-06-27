/**
 * Copyright (c) 2013-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 * @flow
 */

'use strict';

const path = require('path');
const fs = require('fs');
const cp = require('child_process');
const jscodeshift = require('jscodeshift').withParser('flow');
const transform = require('../codemod');

const FIXTURES_DIR = path.join(__dirname, './fixtures');

fs.readdirSync(FIXTURES_DIR).forEach(fixture => {
  test(fixture, () => {
    const file = path.join(FIXTURES_DIR, fixture);
    const source = fs.readFileSync(file, 'utf8');
    const root = jscodeshift(source);
    const skipped = !transform(jscodeshift, root);
    expect(skipped ? null : root.toSource()).toMatchSnapshot();
  });
});
