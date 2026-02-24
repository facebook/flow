/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../../Tester';

const {generateSimpleTests} = require('../test-utils');
const path = require('path');
const {suite, test} = require('../../../Tester');

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
      'provide quickfix through annotation for invariant subtyping errors',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'add-annotation-for-invariant-subtyping-error.js',
        'quickfix-add-annotation-for-invariant-subtyping-error',
      ),
    ),
    test(
      'provide quickfix for adding optional chaining',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'add-optional-chaining.js',
        'quickfix-add-optional-chaining',
      ),
    ),
    test(
      'provide quickfix for PropMissing errors with dot syntax',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'prop-missing.js',
        'quickfix-prop-missing-dot-syntax',
      ),
    ),
    test(
      'provide quickfix for PropMissing errors with bracket syntax',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'prop-missing-bracket-syntax.js',
        'quickfix-prop-missing-bracket-syntax',
      ),
    ),
    test(
      "don't provide quickfixes for object subtyping errors",
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'object-cast.js',
        'object-cast-no-fix',
      ),
    ),
    test(
      'provide quickfix for type-as-value errors',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'type-as-value.js',
        'quickfix-type-as-value',
      ),
    ),
    test('provide quickfix for aliased ClassObject errors', [
      addFile('lib.js.ignored', 'lib.js'),
      ...generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'class-object-subtype.js',
        'quickfix-class-object',
      ),
    ]),

    test(
      'provide codeAction for MethodUnbinding errors',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'method-unbinding.js',
        'quickfix-method-unbinding',
      ),
    ),
    test(
      'provide quickfix for unused promise errors',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'fix-unused-promise.js',
        'quickfix-unused-promise',
      ),
    ),
    test('provide autoimport for missing import', [
      addFile('import-provider.js.ignored', 'import-provider.js'),
      ...generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'fix-missing-import.js',
        'quickfix-auto-import',
      ),
    ]),
    test(
      'provide quickfix for class member access',
      generateSimpleTests(
        'quickfix',
        {
          addFile,
          lspIgnoreStatusAndCancellation,
          lspStartAndConnect,
          lspRequestAndWaitUntilResponse,
        },
        __dirname,
        'fix-class-member-access.js',
        'quickfix-class-member-access',
      ),
    ),
  ],
): SuiteType);
