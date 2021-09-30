/*
 * @flow
 * @format
 */

import type {Suite} from '../../../packages/flow-dev-tools/src/test/Suite';
const {suite, test} = require('flow-dev-tools/src/test/Tester');

module.exports = (suite(
  ({
    addFile,
    addFiles,
    addCode,
    lspStartAndConnect,
    lspRequestAndWaitUntilResponse,
    lspIgnoreStatusAndCancellation,
  }) => {
    let verifyRename = function(
      uri: string,
      position: {|line: number, character: number|},
      newName: string,
      response: string,
    ) {
      return lspRequestAndWaitUntilResponse('textDocument/rename', {
        textDocument: {uri: uri},
        position: position,
        newName: newName,
      }).verifyAllLSPMessagesInStep(
        [['textDocument/rename', response]],
        ['textDocument/publishDiagnostics', ...lspIgnoreStatusAndCancellation],
      );
    };
    return [
      test('textDocument/rename of an object property', [
        addFiles('objects.js'),
        lspStartAndConnect(),
        // Rename of an object property
        verifyRename(
          '<PLACEHOLDER_PROJECT_URL>/objects.js',
          {line: 2, character: 20},
          'newName',
          JSON.stringify({
            changes: {
              '<PLACEHOLDER_PROJECT_URL>/objects.js': [
                {
                  range: {
                    start: {line: 2, character: 19},
                    end: {line: 2, character: 22},
                  },
                  newText: 'newName',
                },
                {
                  range: {
                    start: {line: 6, character: 10},
                    end: {line: 6, character: 13},
                  },
                  newText: 'newName: bar',
                },
                {
                  range: {
                    start: {line: 10, character: 8},
                    end: {line: 10, character: 13},
                  },
                  newText: '{newName: bar}',
                },
              ],
            },
          }),
        ),
      ]),
      test('textDocument/rename of a local variable used in object shorthand', [
        addFiles('objects.js'),
        lspStartAndConnect(),
        verifyRename(
          '<PLACEHOLDER_PROJECT_URL>/objects.js',
          {line: 5, character: 8},
          'newName',
          JSON.stringify({
            changes: {
              '<PLACEHOLDER_PROJECT_URL>/objects.js': [
                {
                  range: {
                    start: {line: 5, character: 8},
                    end: {line: 5, character: 11},
                  },
                  newText: 'newName',
                },
                {
                  range: {
                    start: {line: 6, character: 10},
                    end: {line: 6, character: 13},
                  },
                  newText: 'bar: newName',
                },
              ],
            },
          }),
        ),
      ]),
      test('textDocument/rename of a var bound by destructuring shorthand', [
        addFiles('objects.js'),
        lspStartAndConnect(),
        verifyRename(
          '<PLACEHOLDER_PROJECT_URL>/objects.js',
          {line: 11, character: 9},
          'newName',
          JSON.stringify({
            changes: {
              '<PLACEHOLDER_PROJECT_URL>/objects.js': [
                {
                  range: {
                    start: {line: 10, character: 8},
                    end: {line: 10, character: 13},
                  },
                  newText: '{bar: newName}',
                },
                {
                  range: {
                    start: {line: 11, character: 9},
                    end: {line: 11, character: 12},
                  },
                  newText: 'newName',
                },
              ],
            },
          }),
        ),
      ]),
    ];
  },
): Suite);
