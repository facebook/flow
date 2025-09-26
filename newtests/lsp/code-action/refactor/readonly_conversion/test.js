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
      'provide readonly conversion for array types (in legacy flow syntax)',
      generateSimpleTests(
        'refactor',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'array_type.js',
        'convert_array_type_legacy_flow_syntax',
      ),
    ),
    test(
      'provide readonly conversion for object types (in legacy flow syntax)',
      generateSimpleTests(
        'refactor',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'object_type.js',
        'convert_object_type_legacy_flow_syntax',
      ),
    ),
    test(
      'provide readonly conversion for map and set types (in legacy flow syntax)',
      generateSimpleTests(
        'refactor',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'map_and_set.js',
        'convert_map_and_set_legacy_flow_syntax',
      ),
    ),

    test(
      'provide readonly conversion for array types (in TS syntax)',
      generateSimpleTests(
        'refactor',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'array_type.js',
        'convert_array_type_ts_syntax',
      ),
    ).flowConfig('_flowconfig_ts_syntax'),
    test(
      'provide readonly conversion for object types (in TS syntax)',
      generateSimpleTests(
        'refactor',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'object_type.js',
        'convert_object_type_ts_syntax',
      ),
    ).flowConfig('_flowconfig_ts_syntax'),
    test(
      'provide readonly conversion for map and set types (in TS syntax)',
      generateSimpleTests(
        'refactor',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'map_and_set.js',
        'convert_map_and_set_ts_syntax',
      ),
    ).flowConfig('_flowconfig_ts_syntax'),
  ],
): SuiteType);
