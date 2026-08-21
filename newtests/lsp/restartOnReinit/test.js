/*
 * @flow
 * @format
 */

import type {SuiteType} from '../../Tester';
const {suite, test} = require('../../Tester');

// With `saved_state_restart_on_reinit=true`, a mergebase change makes the server
// exit with `Exit.Restart` and the monitor start a fresh one. The monitor must
// tear the old LSP connection down so the client reconnects under a client id the
// new server knows about; otherwise every later request is answered with nothing
// ("Unknown persistent client N") and the IDE hangs until Flow is restarted.
module.exports = suite(
  ({
    lspStartAndConnect,
    lspNotification,
    lspRequest,
    flowCmd,
    lspIgnoreStatusAndCancellation,
  }) => [
    test('LSP keeps answering requests across a restart-on-reinit', [
      lspStartAndConnect(),
      lspNotification('textDocument/didOpen', {
        textDocument: {
          uri: '<PLACEHOLDER_PROJECT_URL>/open.js',
          languageId: 'javascript',
          version: 1,
          text: `// @flow
function jones(): number { return 15; }
jones();
`,
        },
      })
        .lspRequest('textDocument/definition', {
          textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
          position: {line: 2, character: 1},
        })
        .waitUntilLSPMessage(30000, 'textDocument/definition')
        .verifyAllLSPMessagesInStep(
          [['textDocument/definition', '{open.js,"line":1}']],
          [...lspIgnoreStatusAndCancellation],
        ),

      // Routes the recheck through `reinit_or_restart`. A non-JS target keeps the
      // update set empty, so this is a pure restart with no follow-up recheck.
      flowCmd([
        'force-recheck',
        '--missed-changes',
        '--changed-mergebase',
        'notes.txt',
      ])
        .dontMindServerDeath()
        .waitUntilLSPMessage(60000, 'telemetry/connectionStatus', '{true}'),

      // Bounded `waitUntilLSPMessage` rather than `lspRequestAndWaitUntilResponse`:
      // the latter awaits `sendRequest` with no timeout, so a server that never
      // answers would hang the suite instead of failing it.
      lspRequest('textDocument/definition', {
        textDocument: {uri: '<PLACEHOLDER_PROJECT_URL>/open.js'},
        position: {line: 2, character: 1},
      })
        .waitUntilLSPMessage(30000, 'textDocument/definition')
        .verifyAllLSPMessagesInStep(
          [['textDocument/definition', '{open.js,"line":1}']],
          [...lspIgnoreStatusAndCancellation],
        ),
    ]),
  ],
) as SuiteType;
