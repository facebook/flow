/**
 * @format
 */

'use strict';

const path = require('path');
const fs = require('fs');
const cp = require('child_process');
const jscodeshiftFlowParser = require('jscodeshift/parser/flow');
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
