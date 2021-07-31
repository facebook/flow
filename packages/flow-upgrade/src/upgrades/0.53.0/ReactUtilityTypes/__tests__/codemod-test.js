/**
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *
 * @format
 */

'use strict';

const path = require('path');
const fs = require('fs');
const cp = require('child_process');
const jscodeshift = require('jscodeshift').withParser('flow');
const transform = require('../codemod');

const FIXTURES_DIR = path.join(__dirname, './fixtures');

const HEADERS = [
  // No React.
  [],
  // Default import.
  [`import React from 'react';`],
  // Default import with named imports.
  [`import React, {PropTypes} from 'react';`],
  // Default import, custom name.
  [`import CustomName from 'react';`],
  // Namespace import.
  [`import * as React from 'react';`],
  // Namespace import, custom name.
  [`import * as CustomName from 'react';`],
  // Require.
  [`const React = require('react');`],
  // Require second name.
  [`const React = require('React');`],
  // Require, custom name.
  [`const CustomName = require('react');`],
  // Require, destructured.
  [`const {PropTypes} = require('react');`],
  // Require, destructured and not destructured 1.
  [`const React = require('react');`, `const {PropTypes} = require('react');`],
  // Require, destructured and not destructured 2.
  [`const {PropTypes} = require('react');`, `const React = require('react');`],
  // Destructured `Element`.
  [`const React = require('react');`, `const {Element} = React;`],
  // Named import `Element`.
  [`const React = require('react');`, `import type {Element} from 'react';`],
];

fs.readdirSync(FIXTURES_DIR).forEach(fixture => {
  test(fixture, () => {
    const file = path.join(FIXTURES_DIR, fixture);
    const source = fs.readFileSync(file, 'utf8');
    HEADERS.forEach(header => {
      const root = jscodeshift(`${header.join('\n')}\n\n${source}`);
      const skipped = !transform(jscodeshift, root);
      expect(skipped ? null : root.toSource()).toMatchSnapshot();
    });
  });
});
