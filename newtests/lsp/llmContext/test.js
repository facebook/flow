/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

module.exports = (suite(
  ({
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    addFile,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('basic context request with single file', [
      addFile('sample.js'),
      addFile('utils.js'),
      addFile('types.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('llm/context', {
        editedFilePaths: ['<PLACEHOLDER_PROJECT_URL>/sample.js'],
        environmentDetails: {
          workspaceFolders: [],
          os: 'linux',
        },
        tokenBudget: 1000,
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'llm/context',
            result: {
              llmContext: [
                'IMPORTANT:\n',
                'The type `$ReadOnly<T>` is deprecated, use `Readonly<T>` instead.\n',
                'The type `$ReadOnlyArray<T>` is deprecated, use `ReadonlyArray<T>` instead.\n',
                'The type `mixed` is deprecated, use `unknown` instead.\n',
                '=== File: sample.js ===\n',
                '\n',
                'Imports:\n',
                "  import ... from './types'\n",
                "  import ... from './utils'\n",
                "  import ... from 'react'\n",
                '\n',
                'Declarations:\n',
                '  export default component UserProfile: component UserProfile(age: number, isActive?: boolean, name: string)\n',
                'where\n',
                "'UserProfile' is defined at sample.js:13:16,17:1\n",
                '\n',
              ].join(''),
              filesProcessed: ['sample.js'],
              tokensUsed: 144,
              truncated: false,
            },
          },
        ],
        ['window/showStatus', '$/cancelRequest'],
      ),
    ]),
    test('context request with multiple files', [
      addFile('sample.js'),
      addFile('utils.js'),
      addFile('types.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('llm/context', {
        editedFilePaths: [
          '<PLACEHOLDER_PROJECT_URL>/sample.js',
          '<PLACEHOLDER_PROJECT_URL>/utils.js',
        ],
        environmentDetails: {
          workspaceFolders: [],
          os: 'linux',
        },
        tokenBudget: 2000,
      }).verifyAllLSPMessagesInStep(
        [
          [
            'llm/context',
            '{sample.js,utils.js,filesProcessed,truncated":false}',
          ],
        ],
        [...lspIgnoreStatusAndCancellation],
      ),
    ]),
    test('context request with token budget truncation', [
      addFile('sample.js'),
      addFile('utils.js'),
      addFile('types.js'),
      lspStartAndConnect(),
      lspRequestAndWaitUntilResponse('llm/context', {
        editedFilePaths: [
          '<PLACEHOLDER_PROJECT_URL>/sample.js',
          '<PLACEHOLDER_PROJECT_URL>/utils.js',
          '<PLACEHOLDER_PROJECT_URL>/types.js',
        ],
        environmentDetails: {
          workspaceFolders: [],
          os: 'linux',
        },
        tokenBudget: 50,
      }).verifyAllLSPMessagesInStep(
        [
          {
            method: 'llm/context',
            result: {
              llmContext: [
                'IMPORTANT:\n',
                'The type `$ReadOnly<T>` is deprecated, use `Readonly<T>` instead.\n',
                'The type `$ReadOnlyArray<T>` is deprecated, use `ReadonlyArray<T>` instead.\n',
                'The type `mixed` is deprecated, use `unknown` instead.\n',
              ].join(''),
              filesProcessed: [],
              tokensUsed: 60,
              truncated: true,
            },
          },
        ],
        ['window/showStatus', '$/cancelRequest'],
      ),
    ]),
  ],
): SuiteType);
