/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

const bJsText = `// @flow

import {foo} from './a';

export function useFoo(): number {
  return foo.bar();
}
`;

module.exports = suite(
  ({
    addFile,
    addFiles,
    flowCmd,
    lspStartAndConnect,
    lspNotification,
    lspRequestAndWaitUntilResponse,
    lspIgnoreStatusAndCancellation,
  }) => [
    // A saved-state reinit replaces the whole heap, and a saved state carries no
    // `aloc_table` for any file. The per-connection type-parse-artifacts cache is
    // only invalidated when a recheck *commits*, so a reinit that finds nothing to
    // recheck leaves the cache holding a typed AST built against the old heap. The
    // next hover resolved one of its keyed ALocs against the new heap, found no
    // table, and died with "loc must be concrete" -- permanently, since nothing
    // else would schedule the recheck that clears the cache.
    //
    // `--changed-mergebase --missed-changes` is what an `hg update` looks like to
    // the server. The target is a non-JS path so that the update set stays empty
    // and no recheck follows the reinit; passing a `.js` path would schedule a
    // recheck whose commit clears the cache and hides the bug.
    test('hover survives a saved-state reinit that rechecks nothing', [
      addFiles('a.js', 'b.js'),
      addFile('saved_state_file_changes', '.flow.saved_state_file_changes'),
      flowCmd(['save-state', '--out', '.flow.saved_state']).exitCodes([0]),
      lspStartAndConnect(),
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/b.js',
          languageId: 'javascript',
          version: 1,
          text: bJsText,
        },
      })
        .waitUntilLSPMessage(9000, 'textDocument/publishDiagnostics')
        .verifyAllLSPMessagesInStep(
          [],
          ['window/showStatus', 'textDocument/publishDiagnostics'],
        ),
      // Warms the artifacts cache with a typed AST whose `Foo` reference is a
      // keyed ALoc into a.js.
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/b.js'},
        position: {line: 5, character: 9},
      }).verifyAllLSPMessagesInStep(
        [['textDocument/hover', '{Foo}']],
        [...lspIgnoreStatusAndCancellation, 'textDocument/publishDiagnostics'],
      ),
      flowCmd([
        'force-recheck',
        '--missed-changes',
        '--changed-mergebase',
        'notes.txt',
      ])
        .verifyAllLSPMessagesInStep(
          [],
          [
            ...lspIgnoreStatusAndCancellation,
            'textDocument/publishDiagnostics',
            'telemetry/event',
          ],
        )
        .exitCodes([0]),
      lspRequestAndWaitUntilResponse('textDocument/hover', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/b.js'},
        position: {line: 5, character: 9},
      }).verifyAllLSPMessagesInStep(
        [['textDocument/hover', '{Foo}']],
        [...lspIgnoreStatusAndCancellation, 'textDocument/publishDiagnostics'],
      ),
    ]).lazy('fs'),
  ],
) as SuiteType;
