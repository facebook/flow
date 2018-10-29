/**
 * Copyright 2004-present Facebook. All Rights Reserved.
 *
 * @format
 */
'use strict';

const path = require('path');
const runner = require('../runner.js');

const TEST_PROJ_DIR = path.join(__dirname, './test-project');

test('./test-project', () => {
  console.log = jest.fn();
  const exitCode = runner.run('flow', [TEST_PROJ_DIR]);
  expect(console.log.mock.calls).toMatchSnapshot();
});
