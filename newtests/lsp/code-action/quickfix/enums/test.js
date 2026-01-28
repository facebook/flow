/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../../../Tester';

const {generateSimpleTests} = require('../../test-utils');
const path = require('path');
const {suite, test} = require('../../../../Tester');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspStart,
    lspRequest,
    lspInitializeParams,
    lspRequestAndWaitUntilResponse,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test(
      'provide quickfix for invalid enum member access errors',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'invalid-enum-member-access.js',
        'quickfix-invalid-enum',
      ),
    ),
  ],
): SuiteType);
