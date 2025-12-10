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
    lspRequestAndWaitUntilResponse,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test(
      'provide quickfix for record declaration invalid syntax',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'record-invalid-all.js',
        'quickfix-record-invalid-all',
      ),
    ),
    test(
      'provide quickfix for record declaration with optional property - single',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'record-invalid-optional-single.js',
        'quickfix-record-invalid-optional-single',
      ),
    ),
    test(
      'provide quickfix for record declaration with optional property - full',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'record-invalid-optional-full.js',
        'quickfix-record-invalid-optional-full',
      ),
    ),
    test(
      'provide quickfix for record declaration with variance annotation',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'record-invalid-variance.js',
        'quickfix-record-invalid-variance',
      ),
    ),
    test(
      'provide quickfix for record declaration with semicolon separator',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'record-invalid-semicolon.js',
        'quickfix-record-invalid-semicolon',
      ),
    ),
  ],
): SuiteType);
